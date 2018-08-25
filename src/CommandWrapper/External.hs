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
    )
  where

import Control.Applicative (pure)
import Control.Monad ((>>=))
import Data.Bool (Bool(False))
import Data.Function ((.))
import Data.Functor ((<$>), (<&>), fmap)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty (toList)
import Data.Maybe (Maybe(Just, Nothing), listToMaybe)
import Data.Semigroup ((<>))
import Data.String (String)
import Control.Exception (onException)
import System.Exit (die)
import System.IO (FilePath, IO{-, print-})

import qualified Data.Verbosity as Verbosity (Verbosity(Normal))
import qualified Mainplate (ExternalCommand(..))
import System.Directory
    ( XdgDirectory(XdgConfig)
    , findExecutablesInDirectories
    , getXdgDirectory
    )
import System.FilePath ((</>), (<.>), getSearchPath)
import System.Posix.Process (executeFile)

import qualified CommandWrapper.Config as Global (Config(Config, searchPath))
import qualified CommandWrapper.Environment as Environment -- (Params{-(Params)-})


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
            , verbosity = Verbosity.Normal
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
findSubcommandExecutable subcommands Global.Config{Global.searchPath} = do
    searchPath' <- (searchPath <>) <$> getSearchPath
--  print ("Search path", searchPath')
    loop searchPath' (NonEmpty.toList subcommands)
  where
    loop searchPath' = \case
        [] -> pure Nothing
        (prefix, subcommand) : untriedSubcommands ->
            findSubcommandExecutable' searchPath' subcommand >>= \case
                r@(Just _) -> pure ((prefix, subcommand, ) <$> r)
                Nothing -> loop searchPath' untriedSubcommands

    findSubcommandExecutable' searchPath' =
        fmap listToMaybe . findExecutablesInDirectories searchPath'
