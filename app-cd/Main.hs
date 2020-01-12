-- |
-- Module:      Main
-- Description: CommandWrapper subcommand for changing directory by selecting
--              one from preselected list
-- Copyright:   (c) 2018-2020 Peter Trško
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
import Data.Functor ((<&>))
import qualified Data.List as List (filter, nub, isPrefixOf)
import Data.Maybe (fromMaybe, isJust)
import Data.Monoid (Endo(Endo))
import Data.Semigroup (Semigroup(..))
import Data.String (fromString)
import Data.Void (Void)
import GHC.Generics (Generic)
import System.Environment (getArgs)
import Text.Read (readMaybe)

import Data.Text (Text, isPrefixOf)
import qualified Data.Text as Text (unpack)
import Dhall (FromDhall, ToDhall)
import qualified Dhall (auto)
import Data.Text.Prettyprint.Doc ((<+>))
import qualified Data.Text.Prettyprint.Doc as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Pretty (AnsiStyle)
import qualified Data.Text.Prettyprint.Doc.Util as Pretty (reflow)
import qualified Options.Applicative as Options
    ( Parser
    , long
    , metavar
    , short
    , strArgument
    , strOption
    , switch
    )
import Safe (atMay, lastDef)
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>), normalise)
import System.Posix.Process (executeFile)
import Turtle
    ( Line
    , Shell
    , cd
    , echo
    , env
    , export
    , fromText
    , inproc
    , lineToText
    , need
    , relative
    , select
    , sh
    , testdir
    , unsafeTextToLine
    , unset
    )

import CommandWrapper.Config.Command (SimpleCommand(..))
import CommandWrapper.Config.Environment (EnvironmentVariable(..))
import qualified CommandWrapper.Core.Help.Pretty as Help
import qualified CommandWrapper.Environment as Environment
import CommandWrapper.Message (Result)
import CommandWrapper.Options.Optparse (bashCompleter)
import qualified CommandWrapper.Subcommand.Prelude as CommandWrapper
    ( Shell
    , SubcommandProps(..)
    )
import CommandWrapper.Subcommand.Prelude
    ( SubcommandProps(SubcommandProps)
    , dieWith
    , inputConfig
    , noPreprocessing
    , runSubcommand
    , stderr
    , subcommandParams
    )


data Config = Config
    { directories :: [Text]
    , menuTool :: Maybe (Maybe Text -> SimpleCommand)
    , shell :: Maybe Text
    , terminalEmulator :: Maybe (Text -> Maybe ShellCommand -> SimpleCommand)
    }
  deriving stock (Generic)
  deriving anyclass (Dhall.FromDhall)

-- | Empty 'Config' used when there is no configuration file available.
defConfig :: Config
defConfig = Config
    { directories = []
    , menuTool = Nothing
    , shell = Nothing
    , terminalEmulator = Nothing
    }

data ShellCommand = ShellCommand
    { command :: Text
    , arguments :: [Text]
    }
  deriving stock (Generic)
  deriving anyclass (Dhall.ToDhall)

-- | Smart constructor for 'ShellCommand'.
shellCommand :: Text -> ShellCommand
shellCommand shell = ShellCommand shell []

data Params config = Params
    { config :: config
    , params :: Environment.Params
    , inTmux :: Bool
    , inKitty :: Bool
    }
  deriving stock (Functor, Generic)

main :: IO ()
main = do
    params <- getEnvironment
    arguments <- getArgs
    runSubcommand SubcommandProps
        { preprocess = noPreprocessing
        , doCompletion
        , helpMsg
        , actionOptions = parseOptions
        , defaultAction = Just defMode
        , params
        , arguments
        }
        mainAction

data Mode = Mode
    { strategy :: Strategy
    , query :: Maybe Text
    , directory :: Maybe Text
    }

defMode :: Mode
defMode = Mode
    { strategy = Auto
    , query = Nothing
    , directory = Nothing
    }

mainAction :: Params void -> Mode -> IO ()
mainAction ps@Params{params} Mode{..} = do
    config@Config{..} <- fromMaybe defConfig <$> inputConfig Dhall.auto params
    action <- evalStrategy (config <$ ps) strategy

    sh do
        dir <- case directory of
            Nothing -> do
                let menuToolCommand = case menuTool of
                        Nothing -> defaultMenuTool params
                        Just f  -> f query

                runMenuTool menuToolCommand
                    $ select (unsafeTextToLine <$> List.nub directories)

            Just dir ->
                pure (unsafeTextToLine dir)

        environment <- env
        for_ environment $ \(name, _) ->
            if "COMMAND_WRAPPER_" `isPrefixOf` name
                then unset name
                else pure ()

        executeAction ps dir action
  where
    defaultMenuTool Environment.Params{exePath} = SimpleCommand
        { command = exePath
        , arguments = ["--no-aliases", "config", "--menu"]
        , environment = []
        }

runMenuTool :: SimpleCommand -> Shell Line -> Shell Line
runMenuTool SimpleCommand{..} input = do
    for_ environment $ \EnvironmentVariable{name, value} ->
        export name value

    r <- inproc (fromString command) (fromString <$> arguments) input

    for_ environment $ \EnvironmentVariable{name} ->
        unset name

    pure r

getEnvironment :: IO (Params Void)
getEnvironment = do
    params <- subcommandParams
    Environment.parseEnvIO (dieWith params stderr 1 . fromString . show)
        $ Params (error "This is probably a bug.")
            <$> pure params
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
        case terminalEmulator of
            Nothing ->
                die params 1 "Terminal emulator command is not configured."

            Just cmd ->
                pure (RunTerminalEmulator (`cmd` fmap shellCommand shell))
  where
    Config{shell, terminalEmulator} = config

parseOptions :: Options.Parser (Endo (Maybe Mode))
parseOptions = fmap (Endo . fmap . const) $ go
    <$> shellSwitch
    <*> tmuxSwitch
    <*> kittySwitch
    <*> terminalEmulator
    <*> optional queryOption
    <*> optional dirArgument
  where
    go runShell runTmux runKitty runTerminalEmulator query dir = Mode
        (   (if runShell then ShellOnly else Auto)
            <> (if runTmux then TmuxOnly else Auto)
            <> (if runKitty then KittyOnly else Auto)
            <> (if runTerminalEmulator then TerminalEmulatorOnly else Auto)
        )
        query
        dir

    shellSwitch = Options.switch (Options.long "shell" <> Options.short 's')

    tmuxSwitch = Options.switch (Options.long "tmux" <> Options.short 't')

    kittySwitch = Options.switch (Options.long "kitty" <> Options.short 'k')

    terminalEmulator =
        Options.switch (Options.long "terminal" <> Options.short 'e')

    queryOption = Options.strOption
        (Options.long "query" <> Options.short 'q' <> Options.metavar "QUERY")

    dirArgument = Options.strArgument (Options.metavar "DIRECTORY")

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
        dir <- resolveDirectory

        -- TODO: We should respect verbosity here.
        echo (": cd " <> directory)
        cd directoryPath

        shell <- resolveShell shellOverride
        exportEnvVariables dir (incrementLevel <$> need "CD_LEVEL")
        executeCommand (Text.unpack shell) [] []

    RunTmux shellOverride -> do
        dieIfDirectoryDoesNotExist
        dir <- resolveDirectory
        shell <- resolveShell shellOverride
        let directoryStr = Text.unpack dir
            tmuxOptions =
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
        dir <- resolveDirectory
        shell <- resolveShell shellOverride
        let directoryStr = Text.unpack dir

            -- https://sw.kovidgoyal.net/kitty/remote-control.html#kitty-new-window
            kittyOptions =
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
        dir <- resolveDirectory

        let SimpleCommand{..} = term directoryText
        exportEnvVariables dir (pure "0")
        executeCommand command arguments environment
  where
    directoryPath = fromText directoryText
    directoryText = lineToText directory

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

    resolveDirectory
      | Turtle.relative directoryPath =
            liftIO getCurrentDirectory <&> \cwd ->
                fromString $ normalise (cwd </> Text.unpack directoryText)
      | otherwise =
            pure directoryText

    executeCommand command arguments environment = do
        -- TODO: We should respect verbosity here.
        echo $ ": " <> showCommand command arguments

        for_ environment $ \EnvironmentVariable{name, value} ->
            export name value

        liftIO $ executeFile command True arguments Nothing
            `onException`
                die params 126
                    ("'" <> fromString command <> "': Failed to execute.")

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
    exportEnvVariables :: Text -> Shell Text -> Shell ()
    exportEnvVariables dir getCdLevel = do
        getCdLevel >>= export "CD_LEVEL"
        export "CD_DIRECTORY" dir

    incrementLevel :: Maybe Text -> Text
    incrementLevel =
        maybe "1" (fromString . show . (+1))
        . (>>= readMaybe @Word . Text.unpack)

die :: MonadIO io => Params void -> Int -> Text -> io a
die Params{params} n m = liftIO (dieWith params stderr n m)

doCompletion :: Params a -> Word -> CommandWrapper.Shell -> [String] -> IO ()
doCompletion Params{} index _shell words' = do
    mapM_ putStrLn (List.filter (pat `List.isPrefixOf`) allOptions)
    bashCompleter "directory" "" pat >>= mapM_ putStrLn
  where
    pat = fromMaybe (lastDef "" words') (atMay words' (fromIntegral index))

    allOptions =
        [ "-s", "--shell"
        , "-t", "--tmux"
        , "-k", "--kitty"
        , "-e", "--terminal"
        , "-h", "--help"
        , "-q", "--query="
        ]

helpMsg :: Params a -> Pretty.Doc (Result Pretty.AnsiStyle)
helpMsg Params{params = Environment.Params{name, subcommand}} = Pretty.vsep
    [ Pretty.reflow "Change directory by selecting one from preselected list."
    , ""

    , Help.usageSection name
        [ subcommand'
            <+> Pretty.brackets
                    ( Help.longOption "shell"
                    <> "|"
                    <> Help.longOption "tmux"
                    <> "|"
                    <> Help.longOption "kitty"
                    <> "|"
                    <> Help.longOption "terminal"
                    )
            <+> Pretty.brackets (Help.longOptionWithArgument "query" "QUERY")
            <+> Pretty.brackets (Help.metavar "DIRECTORY")

        , subcommand' <+> Help.helpOptions

        , "help" <+> Pretty.brackets (Help.longOption "man") <+> subcommand'
        ]

    , Help.section ("Options" <> ":")
        [ Help.optionDescription ["--shell", "-s"]
            [ Pretty.reflow "Execute a subshell even if in a Tmux session or\
                \ Kitty terminal."
            ]

        , Help.optionDescription ["--tmux", "-t"]
            [ Pretty.reflow "Create a new Tmux window, or fail if not in Tmux\
                \ session."
            ]

        , Help.optionDescription ["--kitty", "-k"]
            [ Pretty.reflow "Create a new Kitty window, or fail if not runing\
                \ in Kitty terminal."
            ]

        , Help.optionDescription ["--terminal", "-e"]
            [ Pretty.reflow "Open a new terminal emulator window."
            ]

        , Help.optionDescription ["--query=QUERY", "-q QUERY"]
            [ Pretty.reflow "Start the search for a directory with the given"
            , Help.metavar "QUERY" <> "."
            ]

        , Help.optionDescription ["--help", "-h"]
            [ Pretty.reflow "Print this help and exit. Same as"
            , Pretty.squotes
                (Help.toolsetCommand name ("help" <+> subcommand')) <> "."
            ]

        , Help.optionDescription ["DIRECTORY"]
            [ "Use", Help.metavar "DIRECTORY"
            , Pretty.reflow "instead of searching for one in a configured list."
            ]

        , Help.globalOptionsHelp name
        ]
    , ""
    ]
  where
    subcommand' = fromString subcommand
