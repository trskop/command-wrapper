{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections #-}
-- |
-- Module:      CommandWrapper.External
-- Description: TODO: Module synopsis
-- Copyright:   (c) 2018 Peter Trško
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
import Data.String (String)
import Data.Traversable (mapM)
import System.Exit (die)
import System.IO (FilePath, IO{-, print-})

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

import qualified CommandWrapper.Config as Global
    ( Config(Config, colourOutput, searchPath, verbosity)
    )
import qualified CommandWrapper.Environment as Environment
    ( AppNames(AppNames, exePath, names, usedName)
    , Params(Params, colour, config, exePath, name, verbosity)
    , commandWrapperEnv
    , getEnv
    , mkEnvVars
    )
import qualified CommandWrapper.Options.ColourOutput as ColourOutput
    ( ColourOutput(Auto)
    )


type Command = Mainplate.ExternalCommand

run :: Environment.AppNames -> Command -> Global.Config -> IO ()
run appNames (Mainplate.ExternalCommand command arguments) =
    executeCommand appNames command arguments

executeCommand
    :: Environment.AppNames
    -- ^ Names and paths under which this instance of @command-wrapper@ is
    -- known.
    -> String
    -- ^ Subcommand name.
    -> [String]
    -- ^ Command line arguments passed to the subcommand.
    -> Global.Config
    -> IO a
executeCommand appNames subcommand arguments globalConfig =
    findSubcommandExecutable commands globalConfig >>= \case
        Nothing ->
            die unableToFindExecutableError

        Just (prefix, command, executable) -> do
--          print ("Trying to execute", executable)
            extraEnvVars <- Environment.mkEnvVars <$> getParams prefix command
            currentEnv <- Environment.getEnv
            let env = Environment.commandWrapperEnv (currentEnv <> extraEnvVars)

            executeFile executable False arguments (Just env)
                `onException` die (unableToExecuteError executable)
  where
    commands = names <&> \prefix ->
        (prefix, prefix <> "-" <> subcommand)

    Environment.AppNames
        { Environment.exePath
        , Environment.usedName
        , Environment.names
        } = appNames

    getParams prefix command = do
        config <- getXdgDirectory XdgConfig (prefix </> command <.> "dhall")
        pure Environment.Params
            { exePath
            , name = usedName
            , config
            , verbosity = Global.verbosity globalConfig
            , colour =
                fromMaybe ColourOutput.Auto (Global.colourOutput globalConfig)
            }

    unableToFindExecutableError =
        "Error: " <> subcommand
        <> ": Unable to find suitable executable for this subcommand"

    unableToExecuteError executable =
        "Error: " <> subcommand
        <> ": Unable to execute external subcommand executable: '"
        <> executable <> "'"

findSubcommandExecutable
    :: NonEmpty (String, String)
    -> Global.Config
    -> IO (Maybe (String, String, FilePath))
findSubcommandExecutable subcommands config = do
    searchPath <- getSearchPath config
--  print ("Search path", searchPath')
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
