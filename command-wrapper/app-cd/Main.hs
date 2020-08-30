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

import Prelude ((+), error, fromIntegral)

import Control.Applicative ((<*>), many, optional, pure)
import Control.Exception (onException)
import Control.Monad ((>>=), (>=>), unless, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bool (Bool(False, True), (&&), not, otherwise)
import Data.Eq ((==))
import Data.Foldable (for_, mapM_)
import Data.Function (($), (.))
import Data.Functor (Functor, (<$), (<$>), (<&>), fmap)
import Data.Int (Int)
import qualified Data.List as List (filter, nub, isPrefixOf)
import Data.Maybe (Maybe(Just, Nothing), fromMaybe, isJust, maybe)
import Data.Monoid (Endo(Endo))
import Data.Ord ((>))
import Data.Semigroup (Semigroup((<>)))
import Data.String (String, fromString)
import Data.Void (Void)
import Data.Word (Word)
import GHC.Generics (Generic)
import System.Environment (getArgs)
import System.IO (FilePath, IO, hIsTerminalDevice, putStrLn, stdout)
import Text.Read (readMaybe)
import Text.Show (show)

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as ByteString (putStrLn)
import Data.ByteString.ShellEscape (Escape, bash, bytes, sh)
import Data.Monoid.Endo (mapEndo)
import Data.Monoid.Endo.Fold (dualFoldEndo)
import Data.Text (Text, isPrefixOf)
import qualified Data.Text as Text (unpack)
import qualified Data.Text.Encoding as Text (encodeUtf8)
import Data.Text.Prettyprint.Doc ((<+>))
import qualified Data.Text.Prettyprint.Doc as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Pretty (AnsiStyle)
import qualified Data.Text.Prettyprint.Doc.Util as Pretty (reflow)
import qualified Options.Applicative as Options
    ( Parser
    , flag'
    , long
    , metavar
    , short
    , strArgument
    , strOption
    )
import Safe (atMay, lastDef)
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>), normalise, takeFileName)
import System.Posix.Process (executeFile)
import qualified Turtle
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

import CommandWrapper.Core.Completion.FileSystem
    ( EntryType(Directory)
    , FileSystemOptions
        ( appendSlashToSingleDirectoryResult
        , entryType
        , expandTilde
        , word
        )
    , defFileSystemOptions
    , queryFileSystem
    )
import CommandWrapper.Core.Config.Environment (EnvironmentVariable(..))
import CommandWrapper.Core.Config.Verbosity (Verbosity(Silent))
import qualified CommandWrapper.Core.Environment as Environment
import qualified CommandWrapper.Core.Help.Pretty as Help
import qualified CommandWrapper.Subcommand.Prelude as CommandWrapper
    ( Shell
    , SubcommandProps(..)
    )
import CommandWrapper.Subcommand.Prelude
    ( Params(Params, name, subcommand, verbosity)
    , Result
    , SubcommandProps(SubcommandProps)
    , dieWith
    , noPreprocessing
    , runSubcommand
    , stderr
    , subcommandParams
    )
import CommandWrapper.Toolset.Config.Command (SimpleCommand(..))

import Config
    ( Config
        ( Config
        , directories
        , menuTool
        , shell
        , terminalEmulator
        )
    , readConfig
    , shellCommand
    )

data Env config = Env
    { config :: config
    , params :: Params
    , inTmux :: Bool
    , inKitty :: Bool
    , inVimTerminal :: Bool
    -- ^ Inside Vim/Neovim terminal buffer.
    }
  deriving stock (Functor, Generic)

main :: IO ()
main = do
    subcommandProps <- do
        params <- getEnvironment
        arguments <- getArgs
        pure SubcommandProps
            { preprocess = noPreprocessing
            , doCompletion
            , helpMsg
            , actionOptions = mapEndo fmap <$> parseOptions
            , defaultAction = Just defMode
            , params
            , arguments
            }

    runSubcommand subcommandProps mainAction

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

mainAction :: Env void -> Mode -> IO ()
mainAction ps@Env{params} Mode{..} = do
    config@Config{..} <- readConfig params
    action <- evalStrategy (config <$ ps) strategy

    Turtle.sh do
        dir <- case directory of
            Nothing -> do
                let menuToolCommand = case menuTool of
                        Nothing -> defaultMenuTool params
                        Just f  -> f query

                runMenuTool menuToolCommand do
                    Turtle.select
                        (Turtle.unsafeTextToLine <$> List.nub directories)

            Just dir ->
                pure (Turtle.unsafeTextToLine dir)

        environment <- Turtle.env
        for_ environment \(name, _) ->
            if "COMMAND_WRAPPER_" `isPrefixOf` name
                then Turtle.unset name
                else pure ()

        executeAction ps dir action
  where
    defaultMenuTool Params{exePath} = SimpleCommand
        { command = exePath
        , arguments = ["--no-aliases", "config", "--menu"]
        , environment = []
        }

runMenuTool
    :: SimpleCommand
    -> Turtle.Shell Turtle.Line
    -> Turtle.Shell Turtle.Line
runMenuTool SimpleCommand{..} input = do
    for_ environment \EnvironmentVariable{name, value} ->
        Turtle.export name value

    r <- Turtle.inproc (fromString command) (fromString <$> arguments) input

    for_ environment \EnvironmentVariable{name} ->
        Turtle.unset name

    pure r

getEnvironment :: IO (Env Void)
getEnvironment = do
    params <- subcommandParams
    Environment.parseEnvIO (dieWith params stderr 1 . fromString . show)
        $ Env (error "This is probably a bug.")
            <$> pure params
            <*> (isJust <$> Environment.optionalVar "TMUX")
            <*> (isJust <$> Environment.optionalVar "KITTY_WINDOW_ID")
            <*> inVimTerminal
  where
    inVimTerminal = do
        haveVim <- isJust <$> Environment.optionalVar "VIM"
        if haveVim
            then isJust <$> Environment.optionalVar "VIMRUNTIME"
            else pure False

data Strategy
    = Auto
    | ShellOnly
    | TmuxOnly
    | KittyOnly
    | TerminalEmulatorOnly
    | ShCommand
    | BashCommand
    | SelfCommand Strategy

instance Semigroup Strategy where
    SelfCommand _ <> s@(SelfCommand _) = s
    _             <> s                 = s

evalStrategy
    :: Env Config
    -> Strategy
    -> IO Action
evalStrategy env@Env{config, inTmux, inKitty, inVimTerminal} = \case
    Auto
      | inTmux && not inVimTerminal ->
            pure (RunTmux shell)

      | inKitty  && not inVimTerminal ->
            pure (RunKitty shell)

      | otherwise -> do
            stdoutIsTerminal <- hIsTerminalDevice stdout
            if stdoutIsTerminal
                then
                    pure (RunShell shell)
                else do
                    name <- takeFileName <$> resolveShell env shell
                    pure if
                      | name == "sh" ->
                            printCdCommand sh
                      | name == "dash" ->
                            printCdCommand sh
                      | name == "bash" ->
                            printCdCommand bash
                      | name == "zsh" ->
                            printCdCommand bash
                      | otherwise ->
                            printCdCommand sh

    ShellOnly ->
        pure (RunShell shell)

    TmuxOnly
      | inTmux    -> pure (RunTmux shell)
      | otherwise ->
        die env 3 "Not in a Tmux session and '--tmux' was specified."

    KittyOnly
      | inKitty   -> pure (RunKitty shell)
      | otherwise ->
        die env 3 "Not runing in Kitty terminal and '--kitty was specified."

    TerminalEmulatorOnly ->
        case terminalEmulator of
            Nothing ->
                die env 1 "Terminal emulator command is not configured."

            Just cmd ->
                pure (RunTerminalEmulator (`cmd` fmap shellCommand shell))

    ShCommand ->
        pure (printCdCommand sh)

    BashCommand ->
        pure (printCdCommand bash)

    SelfCommand strategy -> do
        let toOpt = \case
                Auto ->
                    ""
                ShellOnly ->
                    "--shell "
                TmuxOnly ->
                    "--tmux "
                KittyOnly ->
                    "--kitty "
                TerminalEmulatorOnly ->
                    "--terminal "
                ShCommand ->
                    "--sh-command "
                BashCommand ->
                    "--bash-command "
                SelfCommand s ->
                    toOpt s

        pure $ PrintSelfCommand \dir ->
            toOpt strategy <> bytes (bash (Text.encodeUtf8 dir))

  where
    Config{shell, terminalEmulator} = config

    printCdCommand :: Escape t => (ByteString -> t) -> Action
    printCdCommand f =
        PrintCdCommand \dir -> bytes (f (Text.encodeUtf8 dir))

parseOptions :: Options.Parser (Endo Mode)
parseOptions = dualFoldEndo
    <$> many shellSwitch
    <*> many tmuxSwitch
    <*> many kittySwitch
    <*> many terminalEmulator
    <*> many queryOption
    <*> many shCommand
    <*> many bashCommand
    <*> many selfCommand
    <*> optional dirArgument
  where
    shellSwitch =
        Options.flag' (setStrategy ShellOnly)
            ( Options.long "shell"
            <> Options.short 's'
            )

    tmuxSwitch =
        Options.flag' (setStrategy TmuxOnly)
            ( Options.long "tmux"
            <> Options.short 't'
            )

    kittySwitch =
        Options.flag' (setStrategy KittyOnly)
            ( Options.long "kitty"
            <> Options.short 'k'
            )

    terminalEmulator =
        Options.flag' (setStrategy TerminalEmulatorOnly)
            ( Options.long "terminal"
            <> Options.short 'e'
            )

    shCommand =
        Options.flag' (setStrategy ShCommand)
            ( Options.long "sh-command"
            )

    bashCommand =
        Options.flag' (setStrategy BashCommand)
            ( Options.long "bash-command"
            )

    selfCommand =
        Options.flag' (modifyStrategy SelfCommand)
            ( Options.long "self-command"
            )

    queryOption =
        setQuery <$> Options.strOption
            ( Options.long "query"
            <> Options.short 'q'
            <> Options.metavar "QUERY"
            )
      where
        setQuery q = Endo \mode -> mode{query = Just q}

    setStrategy newStrategy = Endo \mode@Mode{strategy} -> mode
        { strategy = strategy <> newStrategy
        }

    modifyStrategy f = Endo \mode@Mode{strategy} -> mode
        { strategy = strategy <> f strategy
        }

    dirArgument =
        setDirectory <$> Options.strArgument
            ( Options.metavar "DIRECTORY"
            )
      where
        setDirectory directory = Endo \mode -> mode{directory = Just directory}

data Action
    = RunShell (Maybe Text)
    | RunTmux (Maybe Text)
    | RunKitty (Maybe Text)
    | RunTerminalEmulator (Text -> SimpleCommand)
    | PrintCdCommand (Text -> ByteString)
    | PrintSelfCommand (Text -> ByteString)

-- TODO: These actions are very similar, especially 'RunTmux' and 'RunKitty'.
-- We should consider generalising them so that the same code can be used in
-- all cases.  That could turn out to be a way how to make these configurable
-- as well.  I.e. instead of hardcoded Tmux and Kitty options we could have
-- configurable options.
--
-- ```
-- TOOLSET cd --new-window[{=| }{auto|tmux|kitty|…}]
-- ```
executeAction :: Env void -> Turtle.Line -> Action -> Turtle.Shell ()
executeAction env@Env{params} directory = \case
    RunShell shellOverride -> do
        dir <- resolveDirectory

        echo params (": cd " <> directory)
        Turtle.cd directoryPath

        shell <- resolveShell env shellOverride
        exportEnvVariables dir (incrementLevel <$> Turtle.need "CD_LEVEL")
        executeCommand shell [] []

    RunTmux shellOverride -> do
        dir <- resolveDirectory
        shell <- resolveShell env shellOverride
        let directoryStr = Text.unpack dir
            tmuxOptions =
                [ "new-window", "-c", directoryStr
                , "--"
                -- Hadn't found any other reliable way how to pass these
                -- environment variables.
                , "env" , "CD_LEVEL=0" , "CD_DIRECTORY=" <> directoryStr
                , shell
                ]

        executeCommand "tmux" tmuxOptions []

    RunKitty shellOverride -> do
        dir <- resolveDirectory
        shell <- resolveShell env shellOverride
        let directoryStr = Text.unpack dir

            -- https://sw.kovidgoyal.net/kitty/remote-control.html#kitty-new-window
            kittyOptions =
                [ "@", "new-window", "--cwd", directoryStr
                , "--"
                -- Hadn't found any other reliable way how to pass these
                -- environment variables.
                , "env" , "CD_LEVEL=0" , "CD_DIRECTORY=" <> directoryStr
                , shell
                ]

        executeCommand "kitty" kittyOptions []

    RunTerminalEmulator term -> do
        dir <- resolveDirectory

        let SimpleCommand{..} = term directoryText
        exportEnvVariables dir (pure "0")
        executeCommand command arguments environment

    PrintCdCommand escape -> do
        dir <- resolveDirectory
        liftIO (ByteString.putStrLn ("cd " <> escape dir))

    PrintSelfCommand escape -> do
        dir <- resolveDirectory
        let Params{name} = params
        liftIO (ByteString.putStrLn (fromString name <> " cd " <> escape dir))

  where
    directoryPath = Turtle.fromText directoryText
    directoryText = Turtle.lineToText directory

    echo Params{verbosity} msg = when (verbosity > Silent) (Turtle.echo msg)

    resolveDirectory :: MonadIO io => io Text
    resolveDirectory = do
        dieIfDirectoryDoesNotExist
        if
          | Turtle.relative directoryPath ->
                liftIO getCurrentDirectory <&> \cwd ->
                    fromString $ normalise (cwd </> Text.unpack directoryText)
          | otherwise ->
                pure directoryText

    executeCommand command arguments environment = do
        echo params (": " <> showCommand command arguments)

        for_ environment \EnvironmentVariable{name, value} ->
            Turtle.export name value

        liftIO $ executeFile command True arguments Nothing
            `onException`
                die env 126
                    ("'" <> fromString command <> "': Failed to execute.")

    showCommand cmd args = fromString cmd <> " " <> fromString (show args)

    dieIfDirectoryDoesNotExist :: MonadIO io => io ()
    dieIfDirectoryDoesNotExist = do
        exists <- Turtle.testdir directoryPath
        unless exists $ die env 3
            ("'" <> directoryText <> "': Directory doesn't exist.")

    -- TODO:
    --
    -- * Make names of these environment variables configurable.
    -- * Consider also having another variable that will contain a stack of
    --   directories for nested `TOOLSET cd` invocations.
    exportEnvVariables :: Text -> Turtle.Shell Text -> Turtle.Shell ()
    exportEnvVariables dir getCdLevel = do
        getCdLevel >>= Turtle.export "CD_LEVEL"
        Turtle.export "CD_DIRECTORY" dir

    incrementLevel :: Maybe Text -> Text
    incrementLevel =
        maybe "1" (fromString . show . (+ 1))
        . (>>= readMaybe @Word . Text.unpack)

resolveShell :: MonadIO io => Env void -> Maybe Text -> io FilePath
resolveShell env = maybe (Turtle.need "SHELL") (pure . Just) >=> \case
    Nothing ->
        -- TODO: We should probably make a lot more effort discovering
        -- what shell to execute.  Normally something like
        -- `getent passwd $LOGIN` works, however, that's not the best
        -- idea when Kerberos/LDAP/etc. are used.
        die env 3
            "'SHELL': Environment variable is undefined, unable to\
            \ determine what shell to execute."

    Just shell ->
        pure (Text.unpack shell)

die :: MonadIO io => Env void -> Int -> Text -> io a
die Env{params} n m = liftIO (dieWith params stderr n m)

doCompletion :: Env a -> Word -> CommandWrapper.Shell -> [String] -> IO ()
doCompletion Env{} index _shell words' = do
    mapM_ putStrLn (List.filter (pat `List.isPrefixOf`) allOptions)
    queryFileSystem defFileSystemOptions
        { appendSlashToSingleDirectoryResult = True
        , entryType = Just Directory
        , word = pat
        , expandTilde = True
        }
  where
    pat = fromMaybe (lastDef "" words') (atMay words' (fromIntegral index))

    allOptions =
        [ "-s", "--shell"
        , "-t", "--tmux"
        , "-k", "--kitty"
        , "-e", "--terminal"
        , "-h", "--help"
        , "-q", "--query="
        , "--bash-command"
        , "--sh-command"
        , "--self-command"
        ]

helpMsg :: Env a -> Pretty.Doc (Result Pretty.AnsiStyle)
helpMsg Env{params = Params{name, subcommand}} = Pretty.vsep
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
                    <> "|"
                    <> Help.longOption "{bash|sh}-command"
                    <> Pretty.annotate Help.dullGreen "--"
                        <> Pretty.brackets
                            ( Pretty.annotate Help.dullGreen "bash"
                            <> "|"
                            <> Pretty.annotate Help.dullGreen "sh"
                            )
                        <> Pretty.annotate Help.dullGreen "-command"
                    )
            <+> Pretty.brackets (Help.longOptionWithArgument "query" "QUERY")
            <+> Pretty.brackets (Help.metavar "DIRECTORY")
        , subcommand'
            <+> Help.longOption "self-command"
            <+> Pretty.brackets
                    ( Help.longOption "shell"
                    <> "|"
                    <> Help.longOption "tmux"
                    <> "|"
                    <> Help.longOption "kitty"
                    <> "|"
                    <> Help.longOption "terminal"
                    <> "|"
                    <> Help.longOption "{bash|sh}-command"
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

        , Help.optionDescription ["--bash-command"]
            [ Pretty.reflow "Print a command to change directory. If escaping\
                \ is required then Bash string literal is produced. Useful for\
                \ Bash (obviously), Zsh, and other shells that support string\
                \ literals using", Help.value "$'...'", "syntax."
            ]

        , Help.optionDescription ["--sh-command"]
            [ Pretty.reflow "Print a command to change directory. If escaping\
                \ is required then Bourne Shell escaping is used. Useful for\
                \ shells like Dash."
            ]

        , Help.optionDescription ["--self-command"]
            [ Pretty.reflow "Print a command to execute this subcommand with\
                \ selected directory inlined. Very useful for shell shortcut\
                \ bindings."
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
            , Pretty.reflow "instead of asking user to select one from a list."
            ]

        , Help.globalOptionsHelp name
        ]
    , ""
    ]
  where
    subcommand' = fromString subcommand
