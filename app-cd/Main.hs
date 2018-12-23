{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
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
import Data.Foldable (for_)
import qualified Data.List as List (nub)
import Data.Maybe (isJust)
import Data.Semigroup (Semigroup(..))
import Data.String (fromString)
import GHC.Generics (Generic)
import System.Exit (die)
import Text.Read (readMaybe)

import Data.Text (Text, isPrefixOf)
import qualified Data.Text as Text (unpack)
import qualified Dhall (Interpret, auto, inputFile)
import System.Posix.Process (executeFile)
import Turtle
    ( Line
    , Parser
    , Shell
    , cd
    , echo
    , env
    , export
    , fromText
    , inproc
    , liftIO
    , lineToText
    , need
    , options
    , select
    , switch
    , sh
    , testdir
    , unsafeTextToLine
    , unset
    )

import CommandWrapper.Config.Command (SimpleCommand(..))
import CommandWrapper.Config.Environment (EnvironmentVariable(..))
import qualified CommandWrapper.Environment as Environment


-- TODO:
--
--   * Support for glob patterns in configuration? Would be useful for
--     something like `~/Devel/*`, which would list all the immediate
--     subdirectories of ~/Devel
--
--   * Option to detach terminal emulator process. (Spawn a new process, and
--     let the parent die. Make sure that stdin/stdout are detached, and
--     killing original process where `TOOLSET cd` was invoked won't kill the
--     terminal.)
--
--   * Option to print a command that would be performed.  This is useful for
--     shell key-bindings.  Printing plain `cd ${dir}` would be nice as well.

data Config = Config
    { directories :: [Text]
    , menuTool :: SimpleCommand
    , shell :: Text
    , terminalEmulator :: Text -> SimpleCommand
    }
  deriving (Generic)

instance Dhall.Interpret Config

main :: IO ()
main = do
    (Environment.Params{config = configFile}, inTmuxSession) <- getEnvironment

    strategy <- options description parseOptions
    Config{..} <- Dhall.inputFile Dhall.auto configFile
    action <- evalStrategy terminalEmulator shell inTmuxSession strategy

    sh $ do
        dir <- runMenuTool menuTool
            $ select (unsafeTextToLine <$> List.nub directories)

        environment <- env
        for_ environment $ \(name, _) ->
            if "COMMAND_WRAPPER_" `isPrefixOf` name
                then unset name
                else pure ()

        executeAction dir action
  where
    description =
        "Change directory by selecting one from preselected list"

runMenuTool :: SimpleCommand -> Shell Line -> Shell Line
runMenuTool SimpleCommand{..} input = do
    for_ environment $ \EnvironmentVariable{name, value} ->
        export name value

    r <- inproc (fromString command) (fromString <$> arguments) input

    for_ environment $ \EnvironmentVariable{name} ->
        unset name

    pure r

getEnvironment :: IO (Environment.Params, Bool)
getEnvironment = Environment.parseEnvIO (die . show)
    $ (,)
        <$> Environment.askParams
        <*> (isJust <$> Environment.optionalVar "TMUX")

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
    :: (Text -> SimpleCommand)
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
    | RunTerminalEmulator (Text -> SimpleCommand)

executeAction :: Line -> Action -> Shell ()
executeAction directory = \case
    RunShell shell -> do
        echo ("+ : cd " <> directory)
        cd directoryPath

        exportEnvVariables (incrementLevel <$> need "CD_LEVEL")
        executeCommand (Text.unpack shell) [] []

    RunTmux -> do
        exists <- testdir directoryPath
        unless exists . liftIO
            $ die ("Error: '" <> directoryStr <> "': Directory doesn' exist.")

        -- TODO: Find out how to define pass `CD_LEVEL` and `CD_DIRECTORY` to a
        -- Tmux window.
        executeCommand "tmux" ["new-window", "-c", directoryStr] []

    RunTerminalEmulator term -> do
        exists <- testdir directoryPath
        unless exists . liftIO
            $ die ("Error: '" <> directoryStr <> "': Directory doesn' exist.")

        let SimpleCommand{..} = term directoryText
        exportEnvVariables (pure "0")
        executeCommand command arguments environment
  where
    directoryPath = fromText directoryText
    directoryText = lineToText directory
    directoryStr = Text.unpack directoryText

    executeCommand command arguments environment = do
        echo $ "+ : " <> showCommand command arguments

        for_ environment $ \EnvironmentVariable{name, value} ->
            export name value

        liftIO $ executeFile command True arguments Nothing
            `onException`
                die ("Error: '" <> command <> "': Failed to execute.")

    showCommand cmd args = fromString cmd <> " " <> fromString (show args)

    -- TODO:
    --
    -- * Make names of these environment variables configurable.
    -- * Consider also having another variable that will contain a stack of
    --   directories for nested `TOOLSET cd` invocations.
    exportEnvVariables :: Shell Text -> Shell ()
    exportEnvVariables getCdLevel = do
        getCdLevel >>= export "CD_LEVEL"
        export "CD_DIRECTORY" directoryText

    incrementLevel :: Maybe Text -> Text
    incrementLevel =
        maybe "1" (fromString . show . (+1))
        . (>>= readMaybe @Word . Text.unpack)
