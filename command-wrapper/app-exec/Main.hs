-- |
-- Module:      Main
-- Description: CommandWrapper subcommand for executing commands with a
--              predefined environment.
-- Copyright:   (c) 2018-2020 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- CommandWrapper subcommand for executing commands with a predefined
-- environment. Basically more advanced aliases. Most of the features should
-- come from using Dhall.
module Main (main)
  where

import Prelude ((-), fromIntegral, maxBound, minBound)

import Control.Applicative ((<*>), empty, pure)
import Control.Monad ((>=>), (>>=), when)
import Data.Bool (Bool(False, True), (||), not, otherwise)
import Data.Eq ((==))
import Data.Foldable (any, asum, elem, foldMap, for_, length, mapM_, or)
import Data.Function (($), (.), const, id)
import Data.Functor ((<$>), (<&>), fmap)
import qualified Data.List as List (drop, filter, find, isPrefixOf, take)
import Data.Maybe (Maybe(Just, Nothing), fromMaybe, maybe)
import Data.Monoid (Endo(..), mconcat)
import Data.Ord ((<=))
import Data.Semigroup ((<>))
import Data.String (String, fromString)
import Data.Word (Word)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import System.Environment (getArgs)
import System.Exit (ExitCode(ExitFailure, ExitSuccess), exitWith)
import System.IO (FilePath, IO, putStrLn)
import Text.Show (Show, show)

import qualified Data.CaseInsensitive as CaseInsensitive (mk)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map (delete)
import Data.Text (Text)
import qualified Data.Text as Text (unpack)
import Data.Text.Prettyprint.Doc (Doc, (<+>), pretty)
import qualified Data.Text.Prettyprint.Doc as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Pretty
    ( AnsiStyle
    , Color(Magenta, White)
    , color
    , colorDull
    )
import qualified Data.Text.Prettyprint.Doc.Util as Pretty (reflow)
import Dhall (FromDhall, ToDhall)
import qualified Dhall (auto, inject)
import qualified Dhall.Pretty as Dhall (CharacterSet(Unicode))
import qualified Options.Applicative as Options
    ( Parser
    , auto
    , flag'
    , long
    , option
    , short
    , strOption
    , switch
    , maybeReader
    )
import Safe (atMay, headMay, lastDef)
import qualified System.Clock as Clock (Clock(Monotonic), TimeSpec, getTime)
import qualified System.Posix as Posix
    ( ProcessID
    , ProcessStatus(Exited, Stopped, Terminated)
    , forkProcess
    , getProcessStatus
    )
import qualified Turtle (procs) -- Get rid of dependency on turtle, use process.

import CommandWrapper.Core.Config.Shell (Shell)
import qualified CommandWrapper.Core.Config.Shell as Shell (parse)
import CommandWrapper.Core.Completion.EnvironmentVariables
    ( EnvironmentVariablesOptions(word, prefix)
    , defEnvironmentVariablesOptions
    , queryEnvironmentVariables
    )
import CommandWrapper.Core.Completion.FileSystem
    ( FileSystemOptions
        ( appendSlashToSingleDirectoryResult
        , expandTilde
        , prefix
        , word
        )
    , defFileSystemOptions
    , queryFileSystem
    )
import qualified CommandWrapper.Core.Dhall as Dhall (hPut)
import CommandWrapper.Core.Environment.Variable
    ( defaultCommandWrapperPrefix
    , getCommandWrapperToolsetVarName
    , getCommandWrapperVarName
    , isVariableRemovedBeforeInvokingExternalCommand
    )
import qualified CommandWrapper.Core.Help.Pretty as Help
import CommandWrapper.Core.Message (defaultLayoutOptions, hPutDoc)
import qualified CommandWrapper.Core.Options.Optparse as Options
    ( splitArguments
    , splitArguments'
    )
import qualified CommandWrapper.Subcommand.Prelude (SubcommandProps(..))
import CommandWrapper.Subcommand.Prelude
    ( Params(Params, colour, name, subcommand, verbosity)
    , Result
    , SubcommandProps(SubcommandProps)
    , dieWith
    , inputConfig
    , out
    , stderr
    , stdout
    , subcommandParams
    , runSubcommand
    )
import CommandWrapper.Toolset.Config.Command
    ( Command(..)
    , ExecuteCommandError(..)
    , NamedCommand(..)
    , executeCommand
    , isNamed
    )
import CommandWrapper.Toolset.InternalSubcommand.Help
    ( SubcommandDescription(SubcommandDescription, name, description)
    , TreeAnn(NodeDescription)
    , TreeOptions(TreeOptions, delimiter, showDescription)
    , commandTree
    , treeAnnToAnsi
    )

import DhallInput (defaultDhallInputParams, dhallInput, fromDhallInput)


newtype Config = Config
    { commands :: [NamedCommand]
    -- TODO: Defaults for:
    -- - Notifications
    -- - Command line completion defaults
    }
  deriving stock (Generic)
  deriving anyclass (FromDhall)

-- | Empty 'Config' used when there is no configuration file available.
defConfig :: Config
defConfig = Config
    { commands = []
    }

data Action
    = List
    | Tree
    | DryRun
    | DryRunCompletion Word Shell
    | Run Bool
    | RunDhall Bool Text

defAction :: Action
defAction = Run False

data ExecParams = ExecParams
    { protocol :: Params
    , config :: Config
    , commandAndItsArguments :: [String]
    }
  deriving stock (Generic)

main :: IO ()
main = do
    props <- subcommandProps
    runSubcommand props \ExecParams{..} -> \case
        List ->
            listCommands protocol config ListOptions{showDescription = True}

        Tree ->
            showCommandTree protocol config TreeOptions
                { delimiter = '.'
                , showDescription = True
                }

        DryRun ->
            getExecutableCommand protocol config commandAndItsArguments
                >>= printAsDhall protocol

        DryRunCompletion idx shell -> do
            getCompletionCommand protocol config shell idx commandAndItsArguments
                >>= printAsDhall protocol

        Run monitor ->
            getExecutableCommand protocol config commandAndItsArguments
                >>= executeAndMonitorCommand protocol MonitorOptions
                        { monitor
                        , notificationMessage = "Action "
                            <> maybe "" (\s -> "'" <> fromString s <> "'")
                                (headMay commandAndItsArguments)
                        }

        RunDhall monitor expression ->
            runDhall protocol config
                MonitorOptions
                    { monitor
                    , notificationMessage = "Action "
                    }
                RunDhallOptions
                    { expression

                    -- There is no command name, only arguments.
                    , arguments = fromString <$> commandAndItsArguments
                    }
  where
    subcommandProps :: IO (SubcommandProps ExecParams Action)
    subcommandProps = do
        protocol <- subcommandParams
        arguments <- getArgs
        possiblyConfig <- inputConfig Dhall.auto protocol

        pure SubcommandProps
            { preprocess = \ep args ->
                let (opts, commandAndItsArguments) = Options.splitArguments args
                in (ep{commandAndItsArguments}, opts)
            , doCompletion
            , helpMsg
            , actionOptions = Endo . fmap <$> parseOptions
            , defaultAction = Just defAction
            , params = ExecParams
                { protocol
                , config = fromMaybe defConfig possiblyConfig
                , commandAndItsArguments = []
                }
            , arguments
            }

    getExecutableCommand :: Params -> Config -> [String] -> IO Command
    getExecutableCommand params Config{commands} commandAndItsArguments =
        case fromString <$> commandAndItsArguments of
            [] ->
                dieWith params stderr 1 "COMMAND: Missing argument."

            name : arguments ->
                getCommand params commands name arguments

    getCompletionCommand
        :: Params
        -> Config
        -> Shell
        -> Word
        -> [String]
        -> IO (Maybe Command)
    getCompletionCommand params config shell index = \case
        [] ->
            dieWith params stderr 1 "COMMAND: Missing argument."

        name : arguments -> do
            mkCmd <- getCompletion params config name
            pure $ mkCmd
                <*> pure shell
                <*> pure (fromIntegral index)
                <*> pure (fromString <$> arguments)

getCommand
    :: Params
    -> [NamedCommand]
    -> Text
    -> [Text]
    -> IO Command
getCommand params@Params{verbosity, colour} commands expectedName arguments =
    case List.find (isNamed expectedName) commands of
        Nothing ->
            dieWith params stderr 126
                $ fromString (show expectedName) <> ": Unknown COMMAND."

        Just NamedCommand{command} ->
            pure (command verbosity colour arguments)

data MonitorOptions = MonitorOptions
    { monitor :: Bool
    , notificationMessage :: Text
    }

executeAndMonitorCommand :: Params -> MonitorOptions -> Command -> IO ()
executeAndMonitorCommand params MonitorOptions{..} command
  | monitor = do
        getDuration <- startDuration
        pid <- Posix.forkProcess (executeCommand' params command)
        exitCode <- getProcessStatus pid
        duration <- getDuration
        out params stdout
            ("Action took: " <> Pretty.viaShow duration <> Pretty.line)
        -- TODO: Value of type `NotifyConfig` should be part of `Config`.
        doNotifyWhen defNotifyConfig True notificationMessage exitCode
        exitWith exitCode

  | otherwise =
        executeCommand' params command
  where
    getProcessStatus :: Posix.ProcessID -> IO ExitCode
    getProcessStatus pid =
        Posix.getProcessStatus True True pid >>= \case
            Just (Posix.Exited exitCode) -> pure exitCode
            Just Posix.Terminated{} -> pure (ExitFailure 1)
            Just Posix.Stopped{} -> getProcessStatus pid
            Nothing -> getProcessStatus pid

data NotifyConfig = NotifyConfig
    { icon :: ExitCode -> Maybe Text
    , urgency :: ExitCode -> Maybe Urgency
    , composeMessage :: Text -> ExitCode -> Text
    , sound :: ExitCode -> Maybe FilePath
    }

defNotifyConfig :: NotifyConfig
defNotifyConfig = NotifyConfig
    { icon = \status ->
        Just if status == ExitSuccess
                then "dialog-information"
                else "dialog-error"

    , urgency = \status ->
        Just if status == ExitSuccess
                then Low
                else Normal

    , composeMessage = \msg status -> mconcat
        [ msg
        , " "
        , if status == ExitSuccess
            then "finished."
            else "failed."
        ]

    , sound = \status ->
        Just if status == ExitSuccess
                then "/usr/share/sounds/freedesktop/stereo/dialog-information.oga"
                else "/usr/share/sounds/freedesktop/stereo/dialog-error.oga"
    }

data Urgency = Low | Normal | Critical

-- TODO:
--
-- - Use Haskell client to send notification instead of relying on external
--   application.
-- - Find another way how to play the message.  Maybe even ask for notification
--   service if it supports sounds, and play sound ourselves only if it doesn't.
-- - Using external play command blocks until the sound is played, obviously,
--   however, this is not desired behaviour for application like this.
doNotifyWhen :: NotifyConfig -> Bool -> Text -> ExitCode -> IO ()
doNotifyWhen NotifyConfig{..} p notificationMessage status = when p do
    execs "notify-send" (iconOpt <> urgencyOpt <> [msgArg])
    for_ (sound status) \soundFile ->
        execs "paplay" [fromString soundFile]
  where
    iconOpt = maybe [] (\i -> ["--icon=" <> i]) (icon status)

    urgencyOpt =
        maybe [] (\u -> ["--urgency=" <> urgencyToText u]) (urgency status)

    msgArg = composeMessage notificationMessage status

    execs cmd args = Turtle.procs cmd args empty

    urgencyToText = \case
        Low -> "low"
        Normal -> "normal"
        Critical -> "critical"

startDuration :: IO (IO Clock.TimeSpec)
startDuration = do
    start <- getTime
    pure do
        end <- getTime
        pure (end - start)
  where
    getTime = Clock.getTime Clock.Monotonic

executeCommand' :: Params -> Command -> IO ()
executeCommand' params = executeCommand fixEnvironment >=> \case
    FailedToSetCurrentDirectory dir e ->
        dieWith params stderr 1 (showing dir <> ": " <> showing e)

    FailedToExecuteCommand cmd' _ _ _ e ->
        dieWith params stderr 126 (showing cmd' <> ": " <> showing e)

  where
    showing :: Show a => a -> Text
    showing = fromString . show

fixEnvironment :: Map String String -> Map String String
fixEnvironment = appEndo
    $ foldMap (Endo . Map.delete . Text.unpack)
        (subcommandVariablesToRemove <> toolsetVariablesToRemove)
  where
    subcommandVariablesToRemove =
        getCommandWrapperVarName defaultCommandWrapperPrefix
        <$> [minBound .. maxBound]

    toolsetVariablesToRemove =
        getCommandWrapperToolsetVarName defaultCommandWrapperPrefix
        <$> List.filter isVariableRemovedBeforeInvokingExternalCommand
            [minBound .. maxBound]

printAsDhall :: ToDhall a => Params -> a -> IO ()
printAsDhall Params{colour} =
    Dhall.hPut colour Dhall.Unicode stdout Dhall.inject

data RunDhallOptions = RunDhallOptions
    { expression :: Text
    , arguments :: [Text]
    }

runDhall :: Params -> Config -> MonitorOptions -> RunDhallOptions -> IO ()
runDhall
  params@Params{colour, verbosity}
  Config{}
  monitorOptions@MonitorOptions{notificationMessage}
  RunDhallOptions{arguments, expression} = do
    -- TODO: Handle dhall exceptions properly.  Get inspired by `config`
    -- subcommand.
    mkCommand
        <- fromDhallInput <$> dhallInput (defaultDhallInputParams expression)
    let cmd@Command{command} = mkCommand verbosity colour arguments
        monitorOptions' = monitorOptions
            { notificationMessage =
                notificationMessage <> "'" <> fromString command <> "'"
            }
    executeAndMonitorCommand params monitorOptions' cmd

newtype ListOptions = ListOptions
    { showDescription :: Bool
    }

data CommandAnn
    = CommandName
    | CommandDescription

listCommands :: Params -> Config -> ListOptions -> IO ()
listCommands Params{colour} Config{commands} ListOptions{..} =
    putDoc $ Pretty.vsep (describe <$> commands) <> Pretty.line
  where
    describe :: NamedCommand -> Doc CommandAnn
    describe NamedCommand{name, description} =
        let commandName =
                Pretty.annotate CommandName (pretty name)

            commandDescription =
                Pretty.annotate CommandDescription (pretty description)

         in if showDescription
                then commandName <+> commandDescription
                else commandName

    putDoc = hPutDoc defaultLayoutOptions toAnsi colour stdout

    toAnsi = \case
        CommandName -> Pretty.color Pretty.Magenta
        CommandDescription -> Pretty.colorDull Pretty.White

showCommandTree :: Params -> Config -> TreeOptions -> IO ()
showCommandTree Params{colour} Config{commands} treeOptions =
    putDoc (commandTree treeOptions descriptions <> Pretty.line)
  where
    descriptions :: [SubcommandDescription Text (Maybe (Doc TreeAnn))]
    descriptions = commands <&> \NamedCommand{name, description} ->
        SubcommandDescription
            { name = name
            , description =
                Pretty.annotate NodeDescription . pretty <$> description
            }

    putDoc = hPutDoc defaultLayoutOptions treeAnnToAnsi colour stdout

parseOptions :: Options.Parser (Action -> Action)
parseOptions = asum
    [ Options.flag' (const (Run True)) (Options.long "notify")
    , expressionOption <*> Options.switch (Options.long "notify")
    , Options.flag' (const List) $ mconcat
        [ Options.short 'l'
        , Options.long "list"
        , Options.long "ls"
        ]
    , Options.flag' (const Tree) (Options.long "tree" <> Options.short 't')
    , Options.flag' (\i s _ -> DryRunCompletion i s) (Options.long "print-completion")
        <*> indexOption
        <*> shellOption
    , Options.flag' (const DryRun) (Options.long "print")

    , pure id
    ]
  where
    expressionOption =
        Options.strOption (Options.long "expression") <&> \expr m _ ->
            RunDhall m expr

    indexOption :: Options.Parser Word
    indexOption = Options.option Options.auto (Options.long "index")

    shellOption :: Options.Parser Shell
    shellOption = Options.option
        (Options.maybeReader (Shell.parse . CaseInsensitive.mk))
        (Options.long "shell")

doCompletion :: ExecParams -> Word -> Shell -> [String] -> IO ()
doCompletion execParams index shell _ =
    case Options.splitArguments' words of
        (_, _, []) ->
            completeExecSubcommand

        (_, indexBound, commandName : commandArguments)
          | not canCompleteCommand || index <= indexBound ->
                completeExecSubcommand
          | otherwise ->
                doCommandCompletion protocol config shell commandName
                    (index - indexBound) commandArguments
  where
    ExecParams
        { protocol
        , config = config@Config{commands}
        , commandAndItsArguments = words
        } = execParams

    completeExecSubcommand :: IO ()
    completeExecSubcommand
      | "--expression=" `List.isPrefixOf` pat = do
            -- TODO: Very similar code can be found in `config` (internal)
            -- subcommand.  Should this be somewhere in `command-wrapper-core`?
            let prefix = "--expression="

                pat' = List.drop (length prefix) pat

                envPrefix = "env:"

                hasPathPrefix = or
                    [ "./" `List.isPrefixOf` pat'
                    , "../" `List.isPrefixOf` pat'
                    , "~" `List.isPrefixOf` pat'
                    ]

            if
              | envPrefix `List.isPrefixOf` pat' ->
                    -- Syntax `env:VARIABLE` is a valid import in Dhall.
                    queryEnvironmentVariables defEnvironmentVariablesOptions
                        { word = List.drop (length envPrefix) pat'
                        , prefix = prefix <> envPrefix
                        }

              | pat' == "." || pat' == ".." ->
                    printMatching ((prefix <>) <$> ["./", "../"])

              | hasPathPrefix ->
                    -- File paths, even `~/some/path`, are valid Dhall
                    -- expressions.  However, relative paths like `foo/bar` are
                    -- not, they need to start with `./` or `../`
                    queryFileSystem defFileSystemOptions
                        { appendSlashToSingleDirectoryResult = True
                        , expandTilde = True
                        , prefix
                        , word = pat'
                        }

              | otherwise ->
                    printMatching ((prefix <>) <$> ["./", "../", "~/", "env:"])

      | hadListOrTreeOrHelp =
            pure ()
      | hadExpression, any (== "--notify") wordsBeforePattern =
            pure ()
      | hadExpression =
            printMatching ["--notify"]
      | hadNotifyOrPrint =
            printMatching commandNames
      | hadPrintCompletion =
            printMatching (printCompletionOptions <> commandNames)
      | otherwise =
            printMatching (allOptions <> commandNames)

    pat = fromMaybe (lastDef "" words) (atMay words (fromIntegral index))

    printMatching = mapM_ putStrLn . List.filter (pat `List.isPrefixOf`)

    wordsBeforePattern = List.take (fromIntegral index) words

    commandNames = Text.unpack . (name :: NamedCommand -> Text) <$> commands

    hadListOrTreeOrHelp = any
        (`elem` ["-l", "--ls", "--list", "-t", "--tree", "-h", "--help"])
        wordsBeforePattern

    hadExpression = any ("--expression=" `List.isPrefixOf`) wordsBeforePattern

    hadNotifyOrPrint = any (`elem` ["--notify", "--print"]) wordsBeforePattern

    hadPrintCompletion = any (== "--print-completion") wordsBeforePattern

    canCompleteCommand = not (hadListOrTreeOrHelp || hadExpression)

    printCompletionOptions =
        ( if any ("--index=" `List.isPrefixOf`) wordsBeforePattern
             then []
             else ["--index="]
        )
        <>  ( if any ("--shell=" `List.isPrefixOf`) wordsBeforePattern
                  then []
                  else ["--shell=" <> s | s <- ["bash", "fish", "zsh"]]
            )

    allOptions =
        [ "-l", "--ls", "--list"
        , "-t", "--tree"
        , "--print"
        , "--print-completion"
        , "--expression="
        , "--notify"
        , "-h", "--help"
        , "--"
        ]

doCommandCompletion
    :: Params
    -> Config
    -> Shell
    -> String
    -- ^ Command name.
    -> Word
    -> [String]
    -> IO ()
doCommandCompletion params config shell commandName index words =
    getCompletion params config commandName >>= \case
        Nothing ->
            defaultCompletion
        Just mkCompletionCommand ->
            executeCommandCompletion params mkCompletionCommand shell index
                words
  where
    -- TODO: Should this be the same default completion as shell does?
    defaultCompletion = pure ()

getCompletion
    :: Params
    -> Config
    -> String
    -> IO (Maybe (Shell -> Natural -> [Text] -> Command))
getCompletion params Config{commands} commandName =
    case List.find (isNamed (fromString commandName)) commands of
        Nothing ->
            dieWith params stderr 1
                ("'" <> fromString commandName <> "': Missing argument.")

        Just NamedCommand{completion} ->
            pure completion

executeCommandCompletion
    :: Params
    -> (Shell -> Natural -> [Text] -> Command)
    -- ^ Function that constructs completion command, i.e. command that will be
    -- executed to provide completion.
    -> Shell
    -> Word
    -> [String]
    -> IO ()
executeCommandCompletion params mkCommand shell index words =
    executeCommand' params
        (mkCommand shell (fromIntegral index) (fromString <$> words))

helpMsg :: ExecParams -> Pretty.Doc (Result Pretty.AnsiStyle)
helpMsg ExecParams{protocol = Params{name, subcommand}} = Pretty.vsep
    [ Pretty.reflow "Execute a command with predefined environment and command\
        \ line options."
    , ""

    , Help.usageSection name
        [ subcommand'
            <+> Pretty.brackets (Help.longOption "notify")
            <+> Pretty.brackets (Help.metavar "--")
            <+> Help.metavar "COMMAND"
            <+> Pretty.brackets (Help.metavar "COMMAND_ARGUMENTS")

        , subcommand' <+> Pretty.braces
            ( Help.longOption "list"
            <> "|"
            <> Help.longOption "ls"
            <> "|"
            <> Help.shortOption 'l'
            <> "|"
            <> Help.longOption "tree"
            <> "|"
            <> Help.shortOption 't'
            )

        , subcommand'
            <+> Help.longOptionWithArgument "expression" "EXPRESSION"
            <+> Pretty.brackets (Help.longOption "notify")
            <+> Pretty.brackets (Help.metavar "--")
            <+> Pretty.brackets (Help.metavar "COMMAND_ARGUMENTS")

        , subcommand'
            <+> Help.longOption "print"
            <+> Pretty.brackets (Help.metavar "--")
            <+> Help.metavar "COMMAND"
            <+> Pretty.brackets (Help.metavar "COMMAND_ARGUMENTS")

        , subcommand'
            <+> Help.longOption "print-completion"
            <+> Help.longOptionWithArgument "index" "NUM"
            <+> Help.longOptionWithArgument "shell" "SHELL"
            <+> Pretty.brackets (Help.metavar "--")
            <+> Help.metavar "COMMAND"
            <+> Pretty.brackets (Help.metavar "COMMAND_ARGUMENTS")

        , subcommand' <+> Help.helpOptions

        , "help" <+> Pretty.brackets (Help.longOption "man") <+> subcommand'
        ]

    , Help.section ("Options" <> ":")
        [ Help.optionDescription ["--list", "--ls", "-l"]
            [ Pretty.reflow "List available", Help.metavar "COMMAND" <> "s."
            ]

        , Help.optionDescription ["--tree", "-t"]
            [ Pretty.reflow "List available", Help.metavar "COMMAND" <> "s"
            , Pretty.reflow
                "in tree-like form treating dots (`.`) as separators."
            ]

        , Help.optionDescription ["--print"]
            [ Pretty.reflow
                "Print command as it will be executed in Dhall format."
            ]

        , Help.optionDescription ["--print-completion"]
            [ "Similar", "to", Help.longOption "print" <> ","
            , Pretty.reflow
                "but prints command that would be used to do command line\
                \ completion if it was invoked.  Additional options"
            , Help.longOptionWithArgument "index" "NUM" <> ",", "and"
            , Help.longOptionWithArgument "shell" "SHELL"
            , Pretty.reflow
                "are passed to the command line completion command."
            ]

        , Help.optionDescription ["--expression=EXPRESSION"]
            [ Pretty.reflow "Execute Dhall", Help.metavar "EXPRESSION" <> "."
            , Pretty.reflow "Expected type of", Help.metavar "EXPRESSION"
            , Pretty.reflow "is documented in"
            , Help.value "command-wrapper-exec(1)"
            , Pretty.reflow "manual page.  It can be viewed by calling"
            , Pretty.squotes
                ( Help.toolsetCommand name
                    ("help" <+> "--man" <+> subcommand')
                )
                <> "."
            ]

        , Help.optionDescription ["--notify"]
            [ Pretty.reflow
                "Send desktop notification when the command is done."
            ]

        , Help.optionDescription ["--help", "-h"]
            [ Pretty.reflow "Print this help and exit. Same as"
            , Pretty.squotes
                (Help.toolsetCommand name ("help" <+> subcommand')) <> "."
            ]

        , Help.optionDescription ["COMMAND"]
            [ Help.metavar "COMMAND" <+> Pretty.reflow "to execute."
            ]

        , Help.optionDescription ["COMMAND_ARGUMENTS"]
            [ Pretty.reflow "Additional arguments passed to"
            <+> Help.metavar "COMMAND" <> "."
            ]

        , Help.globalOptionsHelp name
        ]
    ]
  where
    subcommand' = fromString subcommand
