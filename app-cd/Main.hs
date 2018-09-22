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

import Control.Exception (onException)
import Control.Monad (unless)
import qualified Data.List as List (nub)
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
    , terminalEmulator :: Text -> TerminalEmulator
    }
  deriving (Generic)

instance Dhall.Interpret Config

data TerminalEmulator = TerminalEmulator
    { command :: Text
    , arguments :: [Text]
    }
  deriving (Generic, Show)

instance Dhall.Interpret TerminalEmulator

main :: IO ()
main = do
    (Environment.Params{config = configFile}, inTmuxSession) <- getEnvironment

    strategy <- options description parseOptions
    Config{..} <- Dhall.inputFile Dhall.auto configFile
    action <- evalStrategy terminalEmulator shell inTmuxSession strategy

    sh $ do
        dir <- inproc menuTool []
            $ select (unsafeTextToLine <$> List.nub directories)
        executeAction dir action
  where
    description =
        "Change directory by selecting one from preselected list"

getEnvironment :: IO (Environment.Params, Bool)
getEnvironment = Environment.parseEnvIO (die . show)
    $ (,)
        <$> Environment.askParams
        <*> (isJust <$> Environment.askOptionalVar "TMUX")

data Strategy
    = Auto
    | ShellOnly
    | TmuxOnly
    | TerminalEmulatorOnly

instance Semigroup Strategy where
    Auto <> x    = x
    x    <> Auto = x
    _    <> x    = x

evalStrategy
    :: (Text -> TerminalEmulator)
    -> Text
    -> Bool
    -> Strategy
    -> IO Action
evalStrategy term shell inTmuxSession = \case
    Auto
      | inTmuxSession -> pure RunTmux
      | otherwise     -> pure (RunShell shell)

    ShellOnly -> pure (RunShell shell)

    TmuxOnly
      | inTmuxSession -> pure RunTmux
      | otherwise     ->
        die "Error: Not in a Tmux session and '--tmux' was specified."

    TerminalEmulatorOnly -> pure (RunTerminalEmulator term)

parseOptions :: Parser Strategy
parseOptions =
    go  <$> shellSwitch
        <*> tmuxSwitch
        <*> terminalEmulator
  where
    go runShell noRunShell runTerminalEmulator =
        (if runShell then ShellOnly else Auto)
        <> (if noRunShell then TmuxOnly else Auto)
        <> (if runTerminalEmulator then TerminalEmulatorOnly else Auto)

    shellSwitch =
        switch "shell" 's' "Execute a subshell even if in a Tmux session."

    tmuxSwitch =
        switch "tmux" 't'
            "Create a new Tmux window, or fail if not in Tmux session."

    terminalEmulator =
        switch "terminal" 'e'
            "Open a new terminal emulator window."

data Action
    = RunShell Text
    | RunTmux
    | RunTerminalEmulator (Text -> TerminalEmulator)

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

    RunTerminalEmulator term -> do
        exists <- testdir directoryPath
        unless exists . liftIO
            $ die ("Error: '" <> directoryStr <> "': Directory doesn' exist.")

        let TerminalEmulator{..} = term directoryText
        executeCommand (Text.unpack command) (Text.unpack <$> arguments)
  where
    directoryPath = fromText (lineToText directory)
    directoryText = lineToText directory
    directoryStr = Text.unpack directoryText

    executeCommand command arguments = do
        echo $ "+ : " <> showCommand command arguments

        liftIO $ executeFile command True arguments Nothing
            `onException`
                die ("Error: '" <> command <> "': Failed to execute.")

    showCommand cmd args = fromString cmd <> " " <> fromString (show args)
