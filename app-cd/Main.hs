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
import Control.Monad ((>=>), unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Foldable (for_)
import qualified Data.List as List (nub)
import Data.Maybe (isJust)
import Data.Semigroup (Semigroup(..))
import Data.String (fromString)
import GHC.Generics (Generic)
import Text.Read (readMaybe)

import Data.Text (Text, isPrefixOf)
import qualified Data.Text as Text (unpack)
import qualified Dhall (Inject, Interpret, auto, inputFile)
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
import CommandWrapper.Prelude
    ( HaveCompletionInfo(completionInfoMode)
    , completionInfoFlag
    , dieWith
    , printOptparseCompletionInfoExpression
    , stderr
    , stdout
    , subcommandParams
    )


data Config = Config
    { directories :: [Text]
    , menuTool :: Maybe Text -> SimpleCommand
    , shell :: Maybe Text
    , terminalEmulator :: Text -> Maybe ShellCommand -> SimpleCommand
    }
  deriving stock (Generic)
  deriving anyclass (Dhall.Interpret)

data ShellCommand = ShellCommand
    { command :: Text
    , arguments :: [Text]
    }
  deriving stock (Generic)
  deriving anyclass (Dhall.Inject)

-- | Smart constructor for 'ShellCommand'.
shellCommand :: Text -> ShellCommand
shellCommand shell = ShellCommand shell []

data Params config = Params
    { config :: config
    , params :: Environment.Params
    , inTmux :: Bool
    , inKitty :: Bool
    }
  deriving stock (Functor)

data Mode
    = DefaultMode Strategy (Maybe Text) (Maybe Text)
    | CompletionInfo
    | Completion Word [String]
    | Help

instance HaveCompletionInfo Mode where
  completionInfoMode = const CompletionInfo

main :: IO ()
main = do
    params <- getEnvironment
    options description (completionInfoFlag <*> parseOptions) >>= \case
        DefaultMode strategy query directory ->
            mainAction params strategy query directory

        CompletionInfo ->
            printOptparseCompletionInfoExpression stdout

        Completion _ _ ->
            notYetImplemented params

        Help ->
            notYetImplemented params
  where
    description = "Change directory by selecting one from preselected list."

    notYetImplemented Params{params} =
        dieWith params stderr 125 "Bug: This is not yet implemented."

mainAction :: Params FilePath -> Strategy -> Maybe Text -> Maybe Text -> IO ()
mainAction params@Params{config = configFile} strategy query directory =
  do
    config@Config{..} <- Dhall.inputFile Dhall.auto configFile
    action <- evalStrategy (config <$ params) strategy

    sh $ do
        dir <- case directory of
            Nothing ->
                runMenuTool (menuTool query)
                    $ select (unsafeTextToLine <$> List.nub directories)

            Just dir ->
                pure (unsafeTextToLine dir)

        environment <- env
        for_ environment $ \(name, _) ->
            if "COMMAND_WRAPPER_" `isPrefixOf` name
                then unset name
                else pure ()

        executeAction params dir action

runMenuTool :: SimpleCommand -> Shell Line -> Shell Line
runMenuTool SimpleCommand{..} input = do
    for_ environment $ \EnvironmentVariable{name, value} ->
        export name value

    r <- inproc (fromString command) (fromString <$> arguments) input

    for_ environment $ \EnvironmentVariable{name} ->
        unset name

    pure r

getEnvironment :: IO (Params FilePath)
getEnvironment = do
    params <- subcommandParams
    Environment.parseEnvIO (dieWith params stderr 1 . fromString . show) $ Params
        <$> pure (Environment.config params)
        <*> pure params
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
evalStrategy params@Params{config, inTmux, inKitty} = \case
    Auto
      | inTmux    -> pure (RunTmux shell)
      | inKitty   -> pure (RunKitty shell)
      | otherwise -> pure (RunShell shell)

    ShellOnly ->
        pure (RunShell shell)

    TmuxOnly
      | inTmux    -> pure (RunTmux shell)
      | otherwise ->
        die params 3 "Not in a Tmux session and '--tmux' was specified."

    KittyOnly
      | inKitty   -> pure (RunKitty shell)
      | otherwise ->
        die params 3 "Not runing in Kitty terminal and '--kitty was specified."

    TerminalEmulatorOnly ->
        pure (RunTerminalEmulator (`terminalEmulator` fmap shellCommand shell))
  where
    Config{shell, terminalEmulator} = config

parseOptions :: Parser Mode
parseOptions =
    go  <$> shellSwitch
        <*> tmuxSwitch
        <*> kittySwitch
        <*> terminalEmulator
        <*> optional queryOption
        <*> optional dirArgument
  where
    go runShell runTmux runKitty runTerminalEmulator query dir = DefaultMode
        (   (if runShell then ShellOnly else Auto)
            <> (if runTmux then TmuxOnly else Auto)
            <> (if runKitty then KittyOnly else Auto)
            <> (if runTerminalEmulator then TerminalEmulatorOnly else Auto)
        )
        query
        (fromString . encodeString <$> dir)

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
    = RunShell (Maybe Text)
    | RunTmux (Maybe Text)
    | RunKitty (Maybe Text)
    | RunTerminalEmulator (Text -> SimpleCommand)

-- TODO: These actions are very similar, especially 'RunTmux' and 'RunKitty'.
-- We should consider generalising them so that the same code can be used in
-- all cases.  That could turn out to be a way how to make these configurable
-- as well.  I.e. instead of hardcoded Tmux and Kitty options we could have
-- configurable options.
--
-- ```
-- TOOLSET cd --new-window[{=| }{auto|tmux|kitty|…}]
-- ```
executeAction :: Params void -> Line -> Action -> Shell ()
executeAction params directory = \case
    RunShell shellOverride -> do
        dieIfDirectoryDoesNotExist

        -- TODO: We should respect verbosity here.
        echo (": cd " <> directory)
        cd directoryPath

        shell <- resolveShell shellOverride

        exportEnvVariables (incrementLevel <$> need "CD_LEVEL")
        executeCommand (Text.unpack shell) [] []

    RunTmux shellOverride -> do
        dieIfDirectoryDoesNotExist

        shell <- resolveShell shellOverride
        let tmuxOptions =
                [ "new-window", "-c", directoryStr
                , "--"
                -- Hadn't found any other reliable way how to pass these
                -- environment variables.
                , "env" , "CD_LEVEL=0" , "CD_DIRECTORY=" <> directoryStr
                , Text.unpack shell
                ]

        executeCommand "tmux" tmuxOptions []

    RunKitty shellOverride -> do
        dieIfDirectoryDoesNotExist

        shell <- resolveShell shellOverride
        -- https://sw.kovidgoyal.net/kitty/remote-control.html#kitty-new-window
        let kittyOptions =
                [ "@", "new-window", "--cwd", directoryStr
                , "--"
                -- Hadn't found any other reliable way how to pass these
                -- environment variables.
                , "env" , "CD_LEVEL=0" , "CD_DIRECTORY=" <> directoryStr
                , Text.unpack shell
                ]

        executeCommand "kitty" kittyOptions []

    RunTerminalEmulator term -> do
        dieIfDirectoryDoesNotExist

        let SimpleCommand{..} = term directoryText
        exportEnvVariables (pure "0")
        executeCommand command arguments environment
  where
    directoryPath = fromText directoryText
    directoryText = lineToText directory
    directoryStr = Text.unpack directoryText

--  resolveShell :: MonadIO io => Maybe Text -> io (Maybe Text)
    resolveShell = maybe (need "SHELL") (pure . Just) >=> \case
        Nothing ->
            -- TODO: We should probably make a lot more effort discovering
            -- what shell to execute.  Normally something like
            -- `getent passwd $LOGIN` works, however, that's not the best
            -- idea when Kerberos/LDAP/etc. are used.
            die params 3
                "'SHELL': Environment variable is undefined, unable to\
                \ determine what shell to execute."

        Just shell ->
            pure shell

    executeCommand command arguments environment = do
        -- TODO: We should respect verbosity here.
        echo $ ": " <> showCommand command arguments

        for_ environment $ \EnvironmentVariable{name, value} ->
            export name value

        liftIO $ executeFile command True arguments Nothing
            `onException`
                die params 126 ("'" <> fromString command <> "': Failed to execute.")

    showCommand cmd args = fromString cmd <> " " <> fromString (show args)

    dieIfDirectoryDoesNotExist =  do
        exists <- testdir directoryPath
        unless exists $ die params 3
            ("'" <> directoryText <> "': Directory doesn't exist.")

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

die :: MonadIO io => Params void -> Int -> Text -> io a
die Params{params} n m = liftIO (dieWith params stderr n m)

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
--
-- - Support Neovim remote.  See `yx jmp` for more information.
--
-- - Make it aware of Vim/Neovim terminal, which can prevent us to create new
--   terminal window.
