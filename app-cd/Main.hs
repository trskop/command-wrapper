{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module:      Main
-- Description: CommandWrapper subcommand for changing directory by selecting
--              one from preselected list
-- Copyright:   (c) 2018 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions; POSIX.
--
-- CommandWrapper subcommand for changing directory by selecting one from
-- preselected list.
--
-- It is implemented as external command so that it can be completely
-- overridden if needed.
module Main (main)
  where

import Control.Applicative ((<|>))
import Control.Exception (onException)
import Control.Monad (unless)
import Data.Functor (void)
import Data.Maybe (isJust)
import Data.String (fromString)
import GHC.Generics (Generic)
import System.Exit (die)

import Data.Text (Text)
import qualified Data.Text as Text (unpack)
import qualified Dhall (Interpret, auto, inputFile)
import System.Posix.Process (executeFile)
import Turtle
    ( Line
    , Shell
    , cd
    , echo
    , fromText
    , inproc
    , liftIO
    , lineToText
    , options
    , select
    , sh
    , testdir
    , unsafeTextToLine
    )

import qualified CommandWrapper.Environment as Environment


-- TODO:
--
--   * Support for glob patterns in configuration? Would be useful for
--     something like `~/Devel/*`, which would list all the immediate
--     subdirectories of ~/Devel

data Config = Config
    { directories :: [Text]
    , menuTool :: Text
    , shell :: Text
    }
  deriving (Generic, Show)

instance Dhall.Interpret Config

main :: IO ()
main = do
    (_wrapperName, configFile, tmux) <- Environment.parseEnvIO (die . show) $ do
        void (Environment.askVar "COMMAND_WRAPPER_EXE")
            <|> failInvalidCommandWrapperEnvironment

        (,,)
            <$> Environment.askVar "COMMAND_WRAPPER_NAME"
            <*> Environment.askVar "COMMAND_WRAPPER_CONFIG"
            <*> (isJust <$> Environment.askOptionalVar "TMUX")


    options description (pure ())
    Config{..} <- Dhall.inputFile Dhall.auto configFile

    sh $ do
        line <- inproc menuTool [] $ select (unsafeTextToLine <$> directories)
        executeAction line
            $ if tmux
                  then RunTmux
                  else RunShell shell
  where
    description =
        "Change directory by selecting one from preselected list"

    failInvalidCommandWrapperEnvironment =
        fail "This command must be executed as part of some command-wrapper environment"

data Action
    = RunShell Text
    | RunTmux

executeAction :: Line -> Action -> Shell ()
executeAction directory = \case
    RunShell shell -> do
        echo ("+ : cd " <> directory)
        cd directoryPath
        executeCommand (Text.unpack shell) []

    RunTmux -> do
        exists <- testdir directoryPath
        unless exists . liftIO
            $ die ("Error: '" <> directoryStr <> "': Directory doesn' exist.")
        executeCommand "tmux" ["new-window", "-c", directoryStr]
  where
    directoryPath = fromText (lineToText directory)
    directoryStr = Text.unpack (lineToText directory)

    executeCommand command arguments = do
        echo $ "+ : " <> fromString command <> " " <> fromString (show arguments)
        liftIO $ executeFile command True arguments Nothing
            `onException`
                die ("Error: '" <> command <> "': Failed to execute.")
