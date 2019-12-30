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
import Data.Bool (Bool(True), (&&), not, otherwise)
import Data.Eq (Eq)
import Data.Function ((.), id)
import Data.Functor ((<$>), (<&>), fmap)
import qualified Data.List as List (drop, filter, isPrefixOf, unlines)
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
    , getPermissions
    , pathIsSymbolicLink
    )
import System.FilePath ((</>), splitFileName)


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
    entries <- listEntries wordDir wordPat <&> \case
        -- If there is only one completion option, and it is a directory, by
        -- appending '/' we'll force completion to descend into that directory.
        es@[path]
          | appendSlashToSingleDirectoryResult -> [path <> "/"]
          | otherwise                          -> es

        es -> es

    fmap updateEntry <$> case entryType of
        Just et -> filterM (isEntryType et) entries
        Nothing -> pure entries
  where
    -- Having `word === ""` is a valid case, in which we want to use current
    -- directory and empty pattern for the rest, which is satisified by
    -- `splitFileName` itself:
    --
    --     splitFileName "" === ("./","")
    (wordDir, wordPat) = splitFileName word

    -- While we need "." as a directory to use functions from System.Directory,
    -- but the output needs to be consistent with what user provided.
    --
    -- If `word == "./t"` and `["./tmp", "./test"]` is a match then we want to
    -- output `./tmp` and `./test` entries.  If we have same match, but user
    -- input is `word == "t"` then we want to print `tmp` and `test` entries.
    shouldDropDotSlash =
        "./" `List.isPrefixOf` wordDir && not ("./" `List.isPrefixOf` word)

    updateEntry
      | shouldDropDotSlash = \s -> prefix <> List.drop 2 s <> suffix
      | otherwise          = \s -> prefix <> s <> suffix

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
    -- ^ Prefix of an entry.  This doesn't include the directory part, only the
    -- final entry name.
    -> IO [String]
listEntries dir pat = do
    dirExists <- doesDirectoryExist dir
    if dirExists
        then fmap (dir </>) . filterMatches <$> getDirectoryContents dir
        else pure []
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
