{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:      CommandWrapper.External
-- Description: TODO: Module synopsis
-- Copyright:   (c) 2018-2019 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- TODO: Module description.
module CommandWrapper.External
    ( Command
    , run
    , executeCommand
    , executeCommandWith
    , findSubcommands
    )
  where

import Control.Applicative (pure)
import Control.Exception (onException)
import Control.Monad ((>>=), filterM)
import Data.Bool (Bool(False))
import Data.Foldable (length)
import Data.Function ((.), ($))
import Data.Functor ((<$>), (<&>), fmap)
import qualified Data.List as List (drop, isPrefixOf, nub)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty (toList)
import Data.Maybe (Maybe(Just, Nothing), catMaybes, fromMaybe, listToMaybe)
import Data.Semigroup ((<>))
import Data.String (String, fromString)
import Data.Traversable (mapM)
import System.IO (FilePath, IO, stderr)
import Text.Show (show)

--import qualified Data.Verbosity as Verbosity (Verbosity(Normal))
import qualified Mainplate (ExternalCommand(..))
import System.Directory
    ( XdgDirectory(XdgConfig)
    , doesDirectoryExist
    , executable
    , findExecutablesInDirectories
    , getPermissions
    , getXdgDirectory
    , listDirectory
    )
import System.FilePath ((</>), (<.>), takeFileName)
import qualified System.FilePath as FilePath (getSearchPath)
import System.Posix.Process (executeFile)

import qualified CommandWrapper.Config.Global as Global
    ( Config(Config, colourOutput, searchPath, verbosity)
    )
import qualified CommandWrapper.Environment as Environment
    ( AppNames(AppNames, exePath, names, usedName)
    , Params
        ( Params
        , colour
        , config
        , exePath
        , name
        , subcommand
        , verbosity
        , version
        )
    , commandWrapperEnv
    , getEnv
    , mkEnvVars
    , subcommandProtocolVersion
    )
import qualified CommandWrapper.Options.ColourOutput as ColourOutput
    ( ColourOutput(Auto)
    )
import CommandWrapper.Message
    ( debugMsg
    , dieUnableToExecuteSubcommand
    , dieUnableToFindSubcommandExecutable
    )


type Command = Mainplate.ExternalCommand

run :: Environment.AppNames -> Command -> Global.Config -> IO ()
run appNames (Mainplate.ExternalCommand command arguments) config =
    executeCommand appNames config command arguments

executeCommand
    :: Environment.AppNames
    -- ^ Names and paths under which this instance of @command-wrapper@ is
    -- known.
    -> Global.Config
    -> String
    -- ^ Subcommand name.
    -> [String]
    -- ^ Command line arguments passed to the subcommand.
    -> IO a
executeCommand = executeCommandWith executeFile

executeCommandWith
    :: (FilePath -> Bool -> [String] -> Maybe [(String, String)] -> IO a)
    -- ^ Use this function to execute the subcommand, if found.
    -> Environment.AppNames
    -- ^ Names and paths under which this instance of @command-wrapper@ is
    -- known.
    -> Global.Config
    -> String
    -- ^ Subcommand name.
    -> [String]
    -- ^ Command line arguments passed to the subcommand.
    -> IO a
executeCommandWith f appNames globalConfig subcommand arguments =
    findSubcommandExecutable globalConfig usedName commands >>= \case
        Nothing ->
            dieUnableToFindSubcommandExecutable (fromString usedName) verbosity
                colourOutput stderr (fromString subcommand)

        Just (prefix, command, executable) -> do
            debugMsg (fromString usedName) verbosity colourOutput stderr
                $ "Trying to execute: " <> fromString (show executable)
            extraEnvVars <- Environment.mkEnvVars <$> getParams prefix command
            currentEnv <- Environment.getEnv
            let -- When combining environments with '<>' the first environment
                -- has precedence if the key is in both.  If our environment
                -- already contains Command Wrapper environment variables we
                -- need to override them.
                envBuilder = extraEnvVars <> currentEnv
                env = Environment.commandWrapperEnv appNames envBuilder

            f executable False arguments (Just env)
                `onException` dieUnableToExecuteSubcommand
                    (fromString usedName) verbosity colourOutput stderr
                    (fromString subcommand) (fromString executable)
  where
    commands = names <&> \prefix ->
        (prefix, prefix <> "-" <> subcommand)

    Environment.AppNames{exePath, usedName, names} = appNames

    Global.Config{verbosity, colourOutput = possiblyColourOutput} = globalConfig
    colourOutput = fromMaybe ColourOutput.Auto possiblyColourOutput

    getParams prefix command = do
        config <- getXdgDirectory XdgConfig (prefix </> command <.> "dhall")
        pure Environment.Params
            { exePath
            , name = usedName
            , subcommand
            , config
            , verbosity
            , colour = colourOutput
            , version = Environment.subcommandProtocolVersion
            }

findSubcommandExecutable
    :: Global.Config
    -> String
    -> NonEmpty (String, String)
    -> IO (Maybe (String, String, FilePath))
findSubcommandExecutable config usedName subcommands = do
    searchPath <- getSearchPath config
    debugMsg (fromString usedName) verbosity colourOutput stderr
        $ "Using following subcommand executable search path: "
        <> fromString (show searchPath)

    loop searchPath (NonEmpty.toList subcommands)
  where
    loop searchPath = \case
        [] -> pure Nothing
        (prefix, subcommand) : untriedSubcommands ->
            findSubcommandExecutable' searchPath subcommand >>= \case
                r@(Just _) -> pure ((prefix, subcommand, ) <$> r)
                Nothing -> loop searchPath untriedSubcommands

    findSubcommandExecutable' searchPath =
        fmap listToMaybe . findExecutablesInDirectories searchPath

    Global.Config{verbosity, colourOutput = possiblyColourOutput} = config
    colourOutput = fromMaybe ColourOutput.Auto possiblyColourOutput

-- | Find all (unique) external subcommands.
findSubcommands :: Environment.AppNames -> Global.Config -> IO [String]
findSubcommands Environment.AppNames{Environment.names} config = do
    searchPath <- getSearchPath config
    executablesToSubcommands <$> mapM listDirectoryExecutables searchPath
  where
    prefixes :: NonEmpty String
    prefixes = (<> "-") <$> names

    listDirectoryExecutables :: FilePath -> IO [FilePath]
    listDirectoryExecutables dir = do
        directoryExists <- doesDirectoryExist dir
        if directoryExists
            then listDirectory dir >>= filterM (`isExecutableIn` dir)
            else pure []

    isExecutableIn :: FilePath -> FilePath -> IO Bool
    isExecutableIn file dir = executable <$> getPermissions (dir </> file)

    executablesToSubcommands :: [[FilePath]] -> [String]
    executablesToSubcommands =
        List.nub . (>>= (>>= executableToSubcommand))

    executableToSubcommand :: FilePath -> [String]
    executableToSubcommand filePath =
        catMaybes $ matchSubcommand fileName <$> NonEmpty.toList prefixes
      where
        fileName = takeFileName filePath

    matchSubcommand :: String -> String -> Maybe String
    matchSubcommand fileName prefix =
        if prefix `List.isPrefixOf` fileName
            then Just $ List.drop (length prefix) fileName
            else Nothing

-- | Get search path for subcommands, i.e. list of directories where (external)
-- subcommand executables are expected to be present.
getSearchPath :: Global.Config -> IO [FilePath]
getSearchPath Global.Config{Global.searchPath} =
    (searchPath <>) <$> FilePath.getSearchPath
    -- Search path provided in configuration has precedence to system search
    -- path (value of `PATH` environment variable). This allows users to
    -- override external subcommands if necessary.
