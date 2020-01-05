{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:      CommandWrapper.External
-- Description: TODO: Module synopsis
-- Copyright:   (c) 2018-2020 Peter Trško
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

    -- * Utilities
    , getSearchPath
    , getSubcommandConfigPathToEdit
    )
  where

import Control.Applicative (pure)
import Control.Exception (onException)
import Control.Monad ((>>=), filterM)
import Data.Bool (Bool(False, True), otherwise)
import Data.Foldable (length)
import Data.Function ((.), ($))
import Data.Functor ((<$>), (<&>), fmap)
import qualified Data.List as List (drop, isPrefixOf, nub)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty (toList)
import Data.Maybe (Maybe(Just, Nothing), catMaybes, listToMaybe, maybe)
import Data.Semigroup ((<>))
import Data.String (String, fromString)
import Data.Traversable (mapM)
import System.Exit (ExitCode(ExitFailure), exitWith)
import System.IO (FilePath, IO, stderr)
import Text.Show (show)

import Data.Text.Prettyprint.Doc (pretty, (<+>))
import qualified Data.Text.Prettyprint.Doc as Pretty (colon)
import qualified Data.Text.Prettyprint.Doc.Util as Pretty (reflow)
import qualified Mainplate (ExternalCommand(..))
import System.Directory
    ( doesDirectoryExist
    , doesFileExist
    , executable
    , findExecutablesInDirectories
    , getPermissions
    , listDirectory
    )
import System.FilePath ((</>), (<.>), takeFileName)
import qualified System.FilePath as FilePath (getSearchPath)
import System.Posix.Process (executeFile)

import qualified CommandWrapper.Config.Global as Global
    ( Config(Config, colourOutput, configPaths, searchPath, verbosity)
    , ConfigPaths(ConfigPaths, local, system, user)
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
import CommandWrapper.Message
    ( debugMsg
    , dieUnableToExecuteSubcommand
    , dieUnableToFindSubcommandExecutable
    , errorMsg
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
    commands = commandNames appNames subcommand

    Environment.AppNames{exePath, usedName} = appNames
    Global.Config{colourOutput, configPaths, verbosity} = globalConfig

    getParams prefix command = do
        config
            <- getSubcommandConfigPath' appNames configPaths prefix command
                False

        pure Environment.Params
            { exePath
            , name = usedName
            , subcommand
            , config =
                -- TODO: We should put contents of the file in here.  See
                -- Subcommand Protocol for details.
                maybe "" fromString config
            , verbosity
            , colour = colourOutput
            , version = Environment.subcommandProtocolVersion
            }

-- | Find configuration file for a subcommand.  This function fails if there
-- is no such subcommand, even if there is a configuration file that could
-- potentially be what user was referring to.
getSubcommandConfigPathToEdit
    :: Environment.AppNames
    -> Global.Config
    -> String
    -- ^ Subcommand name.
    -> IO FilePath
getSubcommandConfigPathToEdit appNames globalConfig subcommand =
    findSubcommandExecutable globalConfig usedName commands >>= \case
        Nothing ->
            dieUnableToFindSubcommandExecutable (fromString usedName) verbosity
                colourOutput stderr (fromString subcommand)

        Just (prefix, command, _executable) -> do
            config
                <- getSubcommandConfigPath' appNames configPaths prefix command
                    True

            case config of
                Just file -> do
                    debugMsg (fromString usedName) verbosity colourOutput stderr
                        ( pretty subcommand <> Pretty.colon
                        <+> Pretty.reflow "Subcommand config found:"
                        <+> fromString (show file)
                        )
                    pure file

                Nothing -> do
                    errorMsg (fromString usedName) verbosity colourOutput stderr
                        ( pretty subcommand <> Pretty.colon
                        <+> Pretty.reflow "Unable to find configuration file\
                            \ for this subcommand."
                        )
                    exitWith (ExitFailure 1)
  where
    commands = commandNames appNames subcommand

    Environment.AppNames{usedName} = appNames
    Global.Config{colourOutput, verbosity, configPaths} = globalConfig

getSubcommandConfigPath'
    :: Environment.AppNames
    -> Global.ConfigPaths
    -> String
    -- ^ Prefix of subcommand executable name.
    -> String
    -- ^ Subcommand executable name.
    -> Bool
    -- ^ If 'True' then the purpose is to edit the file, i.e. we want to
    -- provide result even if the configuration file doesn't exists so that it
    -- can be created.
    -> IO (Maybe FilePath)
getSubcommandConfigPath'
  Environment.AppNames{usedName}
  Global.ConfigPaths{local, system, user}
  _prefix
  command
  isForEditing = do
    -- TODO: For some external subcommands it would be useful if we could
    -- fallback to (prefix </> command <.> "dhall")
    --
    -- For example if we file '${config}/yx/command-wrapper-cd.dhall' is
    -- missing then we could default to
    -- '${config}/command-wrapper/command-wrapper-cd.dhall'.
    --
    -- TODO: There is a lot of open questions on how to handle subcommand
    -- config files properly.  Especially if subcommands want allow coexistence
    -- of multiple configuration files.

    let systemConfig = system <&> \dir ->
            dir </> usedName </> command <.> "dhall"

        userConfig = user </> usedName </> command <.> "dhall"

        localConfig = local <&> \dir ->
            dir </> usedName </> command <.> "dhall"

    haveSystemConfig <- maybe (pure False) doesFileExist systemConfig
    haveUserConfig <- doesFileExist userConfig
    haveLocalConfig <- maybe (pure False) doesFileExist localConfig

    pure if
      | haveLocalConfig ->
            localConfig

      | haveUserConfig ->
            Just userConfig

      | haveSystemConfig ->
            systemConfig

      | isForEditing, Just _ <- localConfig ->
            localConfig

      | isForEditing ->
            Just userConfig

      | otherwise ->
            Nothing

-- | Possible executable names for an external subcommand.
commandNames
    :: Environment.AppNames
    -> String
    -- ^ External subcommand that we are looking for.
    -> NonEmpty (String, String)
    -- ^ List of possible subcommand executable names.  First element of the
    -- tuple is prefix (name under which this toolset is known) and the second
    -- one is external subcommand executable name associated with that prefix.
commandNames Environment.AppNames{names} subcommand =
    names <&> \prefix ->
        (prefix, prefix <> "-" <> subcommand)

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

    Global.Config{verbosity, colourOutput} = config

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
