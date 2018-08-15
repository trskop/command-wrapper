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
import Data.Semigroup (Semigroup(..))
import Data.String (fromString)
import GHC.Generics (Generic)
import System.Exit (die)

import Data.Text (Text)
import qualified Data.Text as Text (unpack)
import qualified Dhall (Interpret, auto, inputFile)
import System.Posix.Process (executeFile)
import Turtle
    ( Line
    , Parser
    , Shell
    , cd
    , echo
    , fromText
    , inproc
    , liftIO
    , lineToText
    , options
    , select
    , switch
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
    (_, configFile, inTmuxSession) <- Environment.parseEnvIO (die . show) $ do
        void (Environment.askVar "COMMAND_WRAPPER_EXE")
            <|> failInvalidCommandWrapperEnvironment

        (,,)
            <$> Environment.askVar "COMMAND_WRAPPER_NAME"
            <*> Environment.askVar "COMMAND_WRAPPER_CONFIG"
            <*> (isJust <$> Environment.askOptionalVar "TMUX")


    strategy <- options description parseOptions
    Config{..} <- Dhall.inputFile Dhall.auto configFile
    action <- evalStrategy shell inTmuxSession strategy

    sh $ do
        dir <- inproc menuTool [] $ select (unsafeTextToLine <$> directories)
        executeAction dir action
  where
    description =
        "Change directory by selecting one from preselected list"

    failInvalidCommandWrapperEnvironment =
        fail "This command must be executed as part of some command-wrapper environment"

data Strategy
    = Auto
    | ShellOnly
    | TmuxOnly

instance Semigroup Strategy where
    Auto <> x    = x
    x    <> Auto = x
    _    <> x    = x

evalStrategy :: Text -> Bool -> Strategy -> IO Action
evalStrategy shell inTmuxSession = \case
    Auto
      | inTmuxSession -> pure RunTmux
      | otherwise     -> pure (RunShell shell)

    ShellOnly -> pure (RunShell shell)

    TmuxOnly
      | inTmuxSession -> pure RunTmux
      | otherwise     ->
        die "Error: Not in a Tmux session and '--tmux' was specified."

parseOptions :: Parser Strategy
parseOptions =
    go  <$> shellSwitch
        <*> tmuxSwitch
  where
    go runShell noRunShell =
        (if runShell then ShellOnly else Auto)
        <> (if noRunShell then TmuxOnly else Auto)

    shellSwitch =
        switch "shell" 's' "Execute a subshell even if in a Tmux session."

    tmuxSwitch =
        switch "tmux" 't'
            "Execute a new Tmux window, or fail if not in Tmux session."

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
        echo $ "+ : " <> showCommand command arguments

        liftIO $ executeFile command True arguments Nothing
            `onException`
                die ("Error: '" <> command <> "': Failed to execute.")

    showCommand cmd args = fromString cmd <> " " <> fromString (show args)
