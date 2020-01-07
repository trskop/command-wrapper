{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:      CommandWrapper.Internal.Subcommand.Completion.FileSystem
-- Description: File system entry completion.
-- Copyright:   (c) 2019-2020 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- File system entry completion.
module CommandWrapper.Internal.Subcommand.Completion.FileSystem
    ( FileSystemOptions(..)
    , EntryPattern(..)
    , EntryType(..)
    , parseEntryType
    , showEntryType
    , defFileSystemOptions
    , queryFileSystem
    , fileSystemCompleter

    -- * Helper Functions
    , isEntryType
    , listEntries
    , outputLines
    )
  where

import Prelude (Bounded, Enum, (-))

import Control.Applicative (pure)
import Control.Monad ((>>=), filterM, mapM_)
import Data.Bool (Bool(False, True), (&&), (||), not, otherwise)
import Data.Eq (Eq, (==))
import Data.Function ((.), id)
import Data.Functor ((<$>), (<&>), fmap)
import qualified Data.List as List
    ( drop
    , dropWhile
    , filter
    , isPrefixOf
    , length
    , null
    , uncons
    , unlines
    )
import Data.Maybe (Maybe(Just, Nothing))
import Data.Monoid ((<>))
import Data.String (IsString, String)
import Data.Word (Word)
import GHC.Generics (Generic)
import System.IO (FilePath, IO, putStrLn)
import Text.Show (Show)

import Data.Generics.Product.Typed (typed)
import Data.Output
    ( HasOutput(Output)
    , OutputFile(OutputFile)
    , OutputHandle(OutputHandle, OutputNotHandle)
    , OutputStdoutOrFile
    , pattern OutputStdoutOnly
    )
import qualified Data.Output (HasOutput(output))
import System.AtomicWrite.Writer.String (atomicWriteFile)
import System.Directory
    ( doesDirectoryExist
    , doesFileExist
    , executable
    , getDirectoryContents
    , getHomeDirectory
    , getPermissions
    , listDirectory
    , pathIsSymbolicLink
    , readable
    )
import System.FilePath ((</>), isPathSeparator, splitFileName)


data FileSystemOptions = FileSystemOptions
    { entryType :: Maybe EntryType
    -- ^ Fiter entries by 'EntryType'. 'Nothing' means no filter, list all
    -- entries.
    , entryPattern :: [EntryPattern]
    -- ^ TODO: Currently not supported.
    , word :: String
    , prefix :: String
    , suffix :: String

    , appendSlashToSingleDirectoryResult :: Bool
    -- ^ When single completion is produced and it is a directory should
    -- trailing slash be appended?  'True' means yes, append trailing slash
    -- to single directory completion.

    , allowTilde :: Bool
    -- ^ Should prefix '~' be recognised as home directory? 'True' means yes,
    -- it should be interpreted as a home directory, and 'False' means no, it
    -- has no special meaning.

    , expandTilde :: Bool
    -- ^ Expand tilde in the pattern to home directory even in the string that
    -- represents user input.  This will mean that if user passes '~' it will
    -- always be substituted to home directory the moment user initiates
    -- command line completion.
    --
    -- This argument is ignored if 'allowTilde' is 'False', i.e. when '~'
    -- doesn't have special meaning.

    , output :: OutputStdoutOrFile

-- TODO:
--  , matchingAlgorithm :: MatchingAlgorithm
    }
  deriving stock (Generic, Show)

instance HasOutput FileSystemOptions where
    type Output FileSystemOptions = OutputStdoutOrFile
    output = typed

defFileSystemOptions :: FileSystemOptions
defFileSystemOptions = FileSystemOptions
    { entryType = Nothing
    , entryPattern = []
    , word = ""
    , prefix = ""
    , suffix = ""
    , appendSlashToSingleDirectoryResult = True
    , allowTilde = True
    , expandTilde = False
    , output = OutputStdoutOnly
    }

data EntryPattern
    = Glob String
  deriving stock (Generic, Show)

data EntryType
--  = Command   -- TODO: Last missing piece to get rid of Bash calls.
    = Directory
    | File
    | Executable
    | Symlink
  deriving stock (Bounded, Enum, Generic, Show)

parseEntryType :: (Eq s, IsString s) => s -> Maybe EntryType
parseEntryType = \case
--  "command"    -> Just Command
    "directory"  -> Just Directory
    "file"       -> Just File
    "executable" -> Just Executable
    "symlink"    -> Just Symlink
    _            -> Nothing

showEntryType :: IsString s => EntryType -> s
showEntryType = \case
--  Command    -> "command"
    Directory  -> "directory"
    File       -> "file"
    Executable -> "executable"
    Symlink    -> "symlink"

queryFileSystem :: FileSystemOptions -> IO ()
queryFileSystem opts@FileSystemOptions{output} =
    fileSystemCompleter opts >>= outputLines id output

fileSystemCompleter :: FileSystemOptions -> IO [String]
fileSystemCompleter FileSystemOptions{..} = do
    home <- getHomeDirectory

    let ((wordDirectory, wordPrefix), wordPattern) =
            splitWord home word SplitWordsOptions{allowTilde, expandTilde}

    entries <- listEntries wordDirectory wordPrefix wordPattern
        >>= \case
            -- If there is only one completion option, and it is a directory,
            -- by appending '/' we'll force completion to descend into that
            -- directory.
            --
            -- TODO: There are probably some corner cases that aren't handled
            -- properly, like when we don't want to go deeper into it.
            es@[(path, completion)]
              | appendSlashToSingleDirectoryResult -> do
                    isDir <- doesDirectoryExist path
                    pure if isDir
                        then [(path <> "/", completion <> "/")]
                        else es

              | otherwise ->
                    pure es

            es ->
                pure es

    fmap updateEntry <$> case entryType of
        Just et -> filterM (\(p, _) ->
            -- The value 3 limits the look ahead to 3 directory levels.  The
            -- value was choosen arbitrarily.
            isEntryType 3 et p) entries

        Nothing ->
            pure entries
  where
    updateEntry = \(_, s) -> prefix <> s <> suffix

data SplitWordsOptions = SplitWordsOptions
    { allowTilde :: Bool
    -- ^ Should prefix '~' be recognised as home directory? 'True' means yes,
    -- it should be interpreted as a home directory, and 'False' means no, it
    -- has no special meaning.

    , expandTilde :: Bool
    -- ^ Expand tilde in the pattern to home directory even in the string that
    -- represents user input.  This will mean that if user passes '~' it will
    -- always be substituted to home directory the moment user initiates
    -- command line completion.
    --
    -- This argument is ignored if 'allowTilde' is 'False', i.e. when '~'
    -- doesn't have special meaning.
    }

splitWord
    :: FilePath
    -- ^ Home directory to be subsitituted for '~' if it's interpreted as a
    -- home directory.
    -> String
    -- ^ Word\/pattern to be split.
    -> SplitWordsOptions
    -> ((FilePath, String), String)
    -- ^ Triplet created by splitting word\/pattern into directory part and
    -- pattern part.  Directory part has two flavours, directory name as
    -- recognised by functions from "System.Directory", and second one that is
    -- what closer to what was given to us in word\/pattern.
splitWord home word SplitWordsOptions{..} =
    ((wordDir, wordPrefix), wordPattern)
  where
    -- Value of `shouldPreserveTilde :: Bool` is used to preserve '~' prefix
    -- in `wordPrefix` so that we don't expand it in the user input if
    -- `expandTilde = True`.
    ((wordDir, wordPattern), shouldPreserveTilde) = case word of
        "~" | allowTilde ->
            -- Trailing '/' so that `splitFileName` will keep home directory in
            -- the dir part.
            ((home <> "/", ""), not expandTilde)

        "~/" | allowTilde ->
            -- This has to be a special case due to the following:
            --
            -- >>> splitFileName ("/home/user" </> "")
            -- ("/home/", "user")
            --
            -- Which is the same thing as we would get by the next case.
            ((home <> "/", ""), not expandTilde)

        '~' : '/' : path | allowTilde ->
            (splitFileName (home </> path), not expandTilde)

        path ->
            -- Having `word === ""` is a valid case, in which we want to use
            -- current directory and empty pattern for the rest, which is
            -- satisified by `splitFileName` itself:
            --
            -- >>> splitFileName ""
            -- ("./","")
            (splitFileName path, False)

    -- While we need "." as a directory to use functions from System.Directory,
    -- but the output needs to be consistent with what user provided.
    --
    -- If `word == "./t"` and `["./tmp", "./test"]` is a match then we want to
    -- output `./tmp` and `./test` entries.  If we have same match, but user
    -- input is `word == "t"` then we want to print `tmp` and `test` entries.
    shouldDropDotSlash =
        "./" `List.isPrefixOf` wordDir && not ("./" `List.isPrefixOf` word)

    -- Reconstruct directory path as it was entered by the user, modulo allowed
    -- expansions.
    wordPrefix
      | shouldPreserveTilde =
            "~" </> List.dropWhile isPathSeparator
                    (List.drop (List.length home) wordDir)

      | shouldDropDotSlash =
            List.drop 2 wordDir

      | otherwise =
            wordDir

-- | Predicate to filter file system entries based on their type.  Usage:
--
-- @
-- \\dir pat entryType ->
--     'listEntries' 3 dir pat '>>=' filterM ('isEntryType' et)
-- @
isEntryType
    :: Word
    -- ^ For the predicate to be more reliable we may need to recursively
    -- search directories.  This limits how deeply we search.
    -> EntryType
    -> FilePath
    -> IO Bool
isEntryType searchAheadDepth = \case
    Directory ->
        doesDirectoryExist

    File -> \fp -> do
        isDirectory <- doesDirectoryExist fp
        if isDirectory
            then isNotEmptyDirectory fp
            else doesFileExist fp

    Executable -> \fp -> do
        isDirectory <- doesDirectoryExist fp
        if isDirectory
            then doesDirectoryContainExecutables searchAheadDepth fp
            else doesExecutableExist fp

    Symlink -> \fp -> do
        isDirectory <- doesDirectoryExist fp
        if isDirectory
            then doesDirectoryContainSymlinks searchAheadDepth fp
            else pathIsSymbolicLink fp
  where
    isNotEmptyDirectory fp = do
        isReadable <- readable <$> getPermissions fp
        if isReadable
            then not . List.null <$> listDirectory fp
            else pure True -- We don't know.

    doesExecutableExist fp = do
        fileExists <- doesFileExist fp
        if fileExists
            then executable <$> getPermissions fp
            else pure True -- We don't know.

    doesDirectoryContainExecutables 0 _  = pure True -- We don't know.
    doesDirectoryContainExecutables n fp = do
        isReadable <- readable <$> getPermissions fp
        if isReadable
            then do
                listDirectory fp >>= anyM \name -> do
                    let fp' = fp </> name
                    isDirectory <- doesDirectoryExist fp'
                    if isDirectory
                        then doesDirectoryContainExecutables (n - 1) fp'
                        else doesExecutableExist fp'
            else
                pure True -- We don't know.

    doesDirectoryContainSymlinks 0 _  = pure True  -- We don't know.
    doesDirectoryContainSymlinks n fp = do
        isReadable <- readable <$> getPermissions fp
        if isReadable
            then do
                listDirectory fp >>= anyM \name -> do
                    let fp' = fp </> name
                    isDirectory <- doesDirectoryExist fp'
                    if isDirectory
                        then doesDirectoryContainSymlinks (n - 1) fp'
                        else pathIsSymbolicLink fp'
            else
                pure True -- We don't know.

    anyM p = \case
        [] -> pure False
        x : xs -> p x >>= \c -> if c then pure c else anyM p xs

-- | List file system entries in a specified directory starting with a prefix.
listEntries
    :: FilePath
    -- ^ Directory to be listed.
    -> String
    -- ^ Prefix that is equivalent to directory to be listed, but may differ
    -- from it.
    -> String
    -- ^ Prefix of an entry.  This doesn't include the directory part, only the
    -- final entry name.
    -> IO [(FilePath, String)]
listEntries directory prefix pat = do
    isReadable <- readable <$> getPermissions directory
    if isReadable
        then do
            directoryExists <- doesDirectoryExist directory
            if directoryExists
                then
                    listDirectory' <&> \entries ->
                        filterMatches entries <&> \entry ->
                            (directory </> entry, prefix </> entry)
                else
                    pure []
        else
            pure []
  where
    filterMatches =
        List.filter (pat `List.isPrefixOf`)

    noHiddenFiles = List.filter \s ->
        s == "." || s == ".." || not ("." `List.isPrefixOf` s)

    -- We are trying to be smart about completing hidden files.  When pattern
    -- starts with "." we try to allow special directories like "." and ".." as
    -- well as hidden files.  What we try to avoid are cases like "/./" and
    -- "/../", i.e. when user tries to complete absolute path.
    --
    -- TODO: We may want to make this functionality configurable in the future.
    listDirectory'
      | directory == "/", Just ('.', _) <- List.uncons pat =
            listDirectory directory

      | List.null pat =
            noHiddenFiles <$> listDirectory directory

      | Just ('.', _) <- List.uncons pat =
            getDirectoryContents directory

      | otherwise =
            noHiddenFiles <$> listDirectory directory

-- | Print completion output.
--
-- TODO: Move this into a separate module so that it can be shared when more
-- stuff will be moved out of "CommandWrapper.Internal.Subcommand.Completion"
-- module.
outputLines
    :: (String -> String)
    -- ^ Modify entry before printing it.
    -> OutputStdoutOrFile
    -> [String]
    -> IO ()
outputLines f = \case
    OutputHandle _ ->
        mapM_ (putStrLn . f)

    OutputNotHandle (OutputFile fn) ->
        atomicWriteFile fn . (List.unlines . fmap f)
