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

import Control.Applicative (pure)
import Control.Monad ((>>=))
import Data.Bool (Bool(False))
import Data.Functor ((<$>))
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty (toList)
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

run :: NonEmpty String -> Command -> Global.Config -> IO ()
run appNames (Mainplate.ExternalCommand command arguments) =
    executeCommand ((<> "-") <$> appNames) command arguments

executeCommand
    :: NonEmpty String
    -> String
    -> [String]
    -> Global.Config
    -> IO a
executeCommand prefixes subcommand arguments config =
    findSubcommandExecutable commands config >>= \case
        Nothing ->
            die unableToFindExecutableError

        Just executable -> -- do
--          print ("Trying to execute", executable)
            executeFile executable False arguments Nothing
                `onException` die (unableToExecuteError executable)
  where
    commands = (<> subcommand) <$> prefixes

    unableToFindExecutableError =
        "Error: " <> subcommand
        <> ": Unable to find suitable executable for this subcommand"

    unableToExecuteError executable =
        "Error: " <> subcommand
        <> ": Unable to execute external subcommand executable: '"
        <> executable <> "'"

findSubcommandExecutable :: NonEmpty String -> Global.Config -> IO (Maybe FilePath)
findSubcommandExecutable subcommands Global.Config{Global.searchPath} =
    loop (NonEmpty.toList subcommands)
  where
    loop = \case
        [] -> pure Nothing
        subcommand : untriedSubcommands ->
            findSubcommandExecutable' subcommand >>= \case
                r@(Just _) -> pure r
                Nothing -> loop untriedSubcommands

    findSubcommandExecutable' subcommand = do
        searchPath' <- (searchPath <>) <$> getSearchPath
--      print ("Search path", searchPath')
        listToMaybe <$> findExecutablesInDirectories searchPath' subcommand
