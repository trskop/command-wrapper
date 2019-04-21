-- |
-- Module:      Main
-- Description: CommandWrapper subcommand for changing directory by selecting
--              one from preselected list
-- Copyright:   (c) 2018-2019 Peter Trško
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

import Control.Applicative (optional)
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
    , argPath
    , cd
    , echo
    , encodeString
    , env
    , export
    , fromText
    , inproc
    , liftIO
    , lineToText
    , need
    , optText
    , options
    , select
    , sh
    , switch
    , testdir
    , unsafeTextToLine
    , unset
    )

import CommandWrapper.Config.Command (SimpleCommand(..))
import CommandWrapper.Config.Environment (EnvironmentVariable(..))
import qualified CommandWrapper.Environment as Environment


data Config = Config
    { directories :: [Text]
    , menuTool :: SimpleCommand
    , shell :: Text
    , terminalEmulator :: Text -> SimpleCommand
    }
  deriving stock (Generic)
  deriving anyclass (Dhall.Interpret)

data Params config = Params
    { config :: config
    , inTmux :: Bool
    , inKitty :: Bool
    }
  deriving stock (Functor)

main :: IO ()
main = do
    params@Params{config = configFile} <- getEnvironment

    (strategy, query, directory) <- options description parseOptions
    config@Config{..} <- Dhall.inputFile Dhall.auto configFile
    action <- evalStrategy (config <$ params) strategy

    sh $ do
        dir <- case directory of
            Nothing ->
                runMenuTool menuTool query
                    $ select (unsafeTextToLine <$> List.nub directories)

            Just dir ->
                pure (unsafeTextToLine dir)

        environment <- env
        for_ environment $ \(name, _) ->
            if "COMMAND_WRAPPER_" `isPrefixOf` name
                then unset name
                else pure ()

        executeAction dir action
  where
    description =
        "Change directory by selecting one from preselected list"

runMenuTool :: SimpleCommand -> Maybe Text -> Shell Line -> Shell Line
runMenuTool SimpleCommand{..} _query input = do
    for_ environment $ \EnvironmentVariable{name, value} ->
        export name value

    r <- inproc (fromString command) (fromString <$> arguments) input

    for_ environment $ \EnvironmentVariable{name} ->
        unset name

    pure r

-- | TODO: Refactor this in a way that can reuse "CommandWrapper.Prelude".
getEnvironment :: IO (Params FilePath)
getEnvironment = Environment.parseEnvIO (die . show)
    $ Params
        <$> (Environment.config <$> Environment.askParams)
        <*> (isJust <$> Environment.optionalVar "TMUX")
        <*> (isJust <$> Environment.optionalVar "KITTY_WINDOW_ID")

data Strategy
    = Auto
    | ShellOnly
    | TmuxOnly
    | KittyOnly
    | TerminalEmulatorOnly

instance Semigroup Strategy where
    Auto <> x    = x
    x    <> Auto = x
    _    <> x    = x

evalStrategy
    :: Params Config
    -> Strategy
    -> IO Action
evalStrategy Params{config, inTmux, inKitty} = \case
    Auto
      | inTmux    -> pure RunTmux
      | inKitty   -> pure (RunKitty shell)
      | otherwise -> pure (RunShell shell)

    ShellOnly ->
        pure (RunShell shell)

    TmuxOnly
      | inTmux    -> pure RunTmux
      | otherwise ->
        die "Error: Not in a Tmux session and '--tmux' was specified."

    KittyOnly
      | inKitty   -> pure (RunKitty shell)
      | otherwise ->
        die "Error: Not runing in Kitty terminal and '--kitty was specified."

    TerminalEmulatorOnly ->
        pure (RunTerminalEmulator terminalEmulator)
  where
    Config{shell, terminalEmulator} = config

parseOptions :: Parser (Strategy, Maybe Text, Maybe Text)
parseOptions =
    go  <$> shellSwitch
        <*> tmuxSwitch
        <*> kittySwitch
        <*> terminalEmulator
        <*> optional queryOption
        <*> optional dirArgument
  where
    go runShell runTmux runKitty runTerminalEmulator query dir =
        (   (if runShell then ShellOnly else Auto)
            <> (if runTmux then TmuxOnly else Auto)
            <> (if runKitty then KittyOnly else Auto)
            <> (if runTerminalEmulator then TerminalEmulatorOnly else Auto)
        , query
        , fromString . encodeString <$> dir
        )

    shellSwitch = switch "shell" 's'
        "Execute a subshell even if in a Tmux session or Kitty terminal."

    tmuxSwitch = switch "tmux" 't'
        "Create a new Tmux window, or fail if not in Tmux session."

    kittySwitch = switch "kitty" 'k'
        "Create a new Kitty window, or fail if not runing in Kitty terminal."

    terminalEmulator = switch "terminal" 'e'
        "Open a new terminal emulator window."

    queryOption = optText "query" 'q'
        "Start the search for a directory with the given QUERY."

    dirArgument = argPath "DIR"
        "Use DIR instead of searching for one in a configured list."

data Action
    = RunShell Text
    | RunTmux
    | RunKitty Text
    | RunTerminalEmulator (Text -> SimpleCommand)

executeAction :: Line -> Action -> Shell ()
executeAction directory = \case
    RunShell shell -> do
        echo (": cd " <> directory)
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

    RunKitty shell -> do
        exists <- testdir directoryPath
        unless exists . liftIO
            $ die ("Error: '" <> directoryStr <> "': Directory doesn' exist.")

        -- https://sw.kovidgoyal.net/kitty/remote-control.html#kitty-new-window
        -- TODO: Find out how to define pass `CD_LEVEL` and `CD_DIRECTORY` to a
        -- Kitty window.
        executeCommand "kitty"
            ["@", "new-window", "--cwd", directoryStr, "--", Text.unpack shell]
            []

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
        echo $ ": " <> showCommand command arguments

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

-- TODO:
--
-- - Support for glob patterns in configuration? Would be useful for something
--   like `~/Devel/*`, which would list all the immediate subdirectories of
--   `~/Devel`.
--
-- - Option to detach terminal emulator process. (Spawn a new process, and let
--   the parent die. Make sure that stdin/stdout are detached, and killing
--   original process where `TOOLSET cd` was invoked won't kill the terminal.)
--
-- - Option to print a command that would be performed.  This is useful for
--   shell key-bindings.  Printing plain `cd ${dir}` would be nice as well.
--
-- - Support directory specific commands.  For example we may want a specific
--   shell for a certain directory.
