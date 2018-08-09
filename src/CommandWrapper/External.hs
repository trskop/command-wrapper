{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
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

import Control.Monad ((>>=))
import Data.Bool (Bool(False))
import Data.Functor ((<$>))
import Data.Maybe (Maybe(Just, Nothing), listToMaybe)
import Data.Semigroup ((<>))
import Data.String (String)
import Control.Exception (onException)
import System.Exit (die)
import System.IO (FilePath, IO{-, print-})

import qualified Mainplate (ExternalCommand(..))
import System.Directory (findExecutablesInDirectories)
import System.FilePath (getSearchPath)
import System.Posix.Process (executeFile)

import qualified CommandWrapper.Config as Global (Config(Config, searchPath))


type Command = Mainplate.ExternalCommand

run :: String -> Command -> Global.Config -> IO ()
run appName (Mainplate.ExternalCommand command arguments) =
    executeCommand (appName <> "-") command arguments

executeCommand :: String -> String -> [String] -> Global.Config -> IO a
executeCommand prefix subcommand arguments config =
    findSubcommandExecutable command config >>= \case
        Nothing ->
            die unableToFindExecutableError

        Just executable -> do
--          print ("Trying to execute", executable)
            executeFile executable False arguments Nothing
                `onException` die unableToExecuteError
  where
    command = prefix <> subcommand

    unableToFindExecutableError =
        subcommand <> ": '" <> command
        <> "': Unable to find suitable executable"

    unableToExecuteError =
        subcommand <> ": '" <> command
        <> "': Unable to execute external subcommand"

findSubcommandExecutable :: String -> Global.Config -> IO (Maybe FilePath)
findSubcommandExecutable subcommand Global.Config{Global.searchPath} = do
    searchPath' <- (searchPath <>) <$> getSearchPath
--  print ("Search path", searchPath')
    listToMaybe <$> findExecutablesInDirectories searchPath' subcommand
