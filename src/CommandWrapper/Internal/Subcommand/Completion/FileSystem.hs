{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:      CommandWrapper.Internal.Subcommand.Completion.FileSystem
-- Description: File system entry completion.
-- Copyright:   (c) 2019 Peter Trško
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

import Prelude (Bounded, Enum)

import Control.Applicative (pure)
import Control.Monad ((>>=), filterM, mapM_)
import Data.Bool (Bool(False, True), (&&), not, otherwise)
import Data.Eq (Eq)
import Data.Function ((.), id)
import Data.Functor ((<$>), (<&>), fmap)
import qualified Data.List as List
    ( drop
    , dropWhile
    , filter
    , isPrefixOf
    , length
    , unlines
    )
import Data.Maybe (Maybe(Just, Nothing))
import Data.Monoid ((<>))
import Data.String (IsString, String)
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
    , pathIsSymbolicLink
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
    = Directory
    | File
    | Executable
    | Symlink
  deriving stock (Bounded, Enum, Generic, Show)

parseEntryType :: (Eq s, IsString s) => s -> Maybe EntryType
parseEntryType = \case
    "directory"  -> Just Directory
    "file"       -> Just File
    "executable" -> Just Executable
    "symlink"    -> Just Symlink
    _            -> Nothing

showEntryType :: IsString s => EntryType -> s
showEntryType = \case
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

    entries <- listEntries wordDirectory wordPrefix wordPattern >>= \case
        -- If there is only one completion option, and it is a directory, by
        -- appending '/' we'll force completion to descend into that directory.
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
        Just et -> filterM (\(p, _) -> isEntryType et p) entries
        Nothing -> pure entries
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
--     'listEntries' dir pat '>>=' filterM ('isEntryType' et)
-- @
isEntryType :: EntryType -> FilePath -> IO Bool
isEntryType = \case
    Directory ->
        doesDirectoryExist
    File ->
        doesFileExist
    Executable -> \fp -> do
        fileExists <- doesFileExist fp
        if fileExists
            then executable <$> getPermissions fp
            else pure fileExists
    Symlink ->
        pathIsSymbolicLink

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
    directoryExists <- doesDirectoryExist directory
    if directoryExists
        then
            getDirectoryContents directory <&> \entries ->
                filterMatches entries <&> \entry ->
                    (directory </> entry, prefix </> entry)
        else
            pure []
  where
    filterMatches =
        List.filter (pat `List.isPrefixOf`)

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
