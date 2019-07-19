-- |
-- Module:      Main
-- Description: CommandWrapper subcommand for executing commands with a
--              predefined environment.
-- Copyright:   (c) 2018-2019 Peter Trško
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

import Prelude hiding (words)

import Control.Applicative (empty)
import Control.Monad (when)
import Data.Bifunctor (bimap)
import Data.Foldable (asum, elem, for_)
import Data.Function (on)
import Data.Functor ((<&>))
import qualified Data.List as List
    ( filter
    , find
    , groupBy
    , isPrefixOf
    , sortBy
    , take
    )
import Data.Maybe (fromMaybe)
import Data.Monoid (Endo(..))
import Data.String (fromString)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import System.Environment (getArgs, getEnvironment)
import System.Exit (ExitCode(ExitFailure, ExitSuccess), exitWith)

import qualified Data.CaseInsensitive as CaseInsensitive (mk)
import qualified Data.Map.Strict as Map (delete, fromList, toList)
import Data.Text (Text)
import qualified Data.Text as Text (split, unpack)
import Data.Text.Prettyprint.Doc (Doc, (<+>), pretty)
import qualified Data.Text.Prettyprint.Doc as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Pretty
    ( AnsiStyle
    , Color(Blue, Magenta, White)
    , color
    , colorDull
    )
import qualified Data.Text.Prettyprint.Doc.Util as Pretty (reflow)
import Data.Tree (Forest, Tree(Node), unfoldForest)
import qualified Dhall (Inject, Interpret, auto, inject, input, inputFile)
import qualified Dhall.Pretty as Dhall (CharacterSet(Unicode))
import qualified Options.Applicative as Options
    ( Parser
    , auto
    , defaultPrefs
    , execParserPure
    , flag'
    , handleParseResult
    , info
    , internal
    , long
    , option
    , short
    , strOption
    , switch
    , maybeReader
    )
import Safe (atMay, headMay, lastDef)
import qualified System.Clock as Clock (Clock(Monotonic), TimeSpec, getTime)
import System.Directory (setCurrentDirectory)
import qualified System.Posix as Posix
    ( ProcessID
    , ProcessStatus(Exited, Stopped, Terminated)
    , executeFile
    , forkProcess
    , getProcessStatus
    )
import qualified Turtle (procs) -- Get rid of dependency on turtle, use process.

import CommandWrapper.Config.Command (Command(..), NamedCommand(..))
import qualified CommandWrapper.Config.Command as NamedCommand (isNamed)
import CommandWrapper.Config.Environment (EnvironmentVariable(..))
import qualified CommandWrapper.Config.Environment as EnvironmentVariable
    ( toTuple
    )
import qualified CommandWrapper.Internal.Dhall as Dhall (hPut)
import qualified CommandWrapper.Internal.Subcommand.Help as Help
import CommandWrapper.Message (Result, defaultLayoutOptions, hPutDoc, message)
import qualified CommandWrapper.Options as Options
    ( splitArguments
    , splitArguments'
    )
import qualified CommandWrapper.Options.Shell as Options (Shell)
import qualified CommandWrapper.Options.Shell as Options.Shell (parse)
import CommandWrapper.Prelude
    ( HaveCompletionInfo(completionInfoMode)
    , Params(Params, colour, config, name, subcommand, verbosity)
    , completionInfoFlag
    , dieWith
    , printCommandWrapperStyleCompletionInfoExpression
    , stderr
    , stdout
    , subcommandParams
    )


newtype Config = Config
    { commands :: [NamedCommand]
    -- TODO: Defaults for:
    -- - Notifications
    -- - Command line completion defaults
    }
  deriving stock (Generic)
  deriving anyclass (Dhall.Interpret)

data Action
    = List
    | Tree
    | DryRun
    | DryRunCompletion Word Options.Shell
    | Run Bool
    | RunDhall Bool Text
    | CompletionInfo
    | Completion Word Options.Shell
    | Help

instance HaveCompletionInfo Action where
    completionInfoMode = const CompletionInfo

main :: IO ()
main = do
    params@Params{config = configFile} <- subcommandParams

    (options, commandAndItsArguments) <- Options.splitArguments <$> getArgs

    config <- Dhall.inputFile Dhall.auto configFile
    action <- fmap ($ Run False) . Options.handleParseResult
        -- TODO: Switch to custom parser so that errors are printed correctly.
        $ Options.execParserPure
            Options.defaultPrefs
            (Options.info parseOptions mempty)
            options

    case action of
        List ->
            listCommands params config ListOptions{showDescription = True}

        Tree ->
            showCommandTree params config TreeOptions{}

        DryRun ->
            getExecutableCommand params config commandAndItsArguments
                >>= printAsDhall params

        DryRunCompletion idx shell -> do
            getCompletionCommand params config shell idx commandAndItsArguments
                >>= printAsDhall params

        Run monitor ->
            getExecutableCommand params config commandAndItsArguments
                >>= executeAndMonitorCommand params MonitorOptions
                        { monitor
                        , notificationMessage = "Action "
                            <> maybe "" (\s -> "'" <> fromString s <> "'")
                                (headMay commandAndItsArguments)
                        }

        RunDhall monitor expression ->
            runDhall params config
                MonitorOptions
                    { monitor
                    , notificationMessage = "Action "
                    }
                RunDhallOptions
                    { expression

                    -- There is no command name, only arguments.
                    , arguments = fromString <$> commandAndItsArguments
                    }

        CompletionInfo ->
            printCommandWrapperStyleCompletionInfoExpression stdout

        Completion index shell ->
            doCompletion params config shell index commandAndItsArguments

        Help ->
            let Params{verbosity, colour} = params
             in message defaultLayoutOptions verbosity colour stdout
                    (helpMsg params)
  where
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
        -> Options.Shell
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
    case List.find (NamedCommand.isNamed expectedName) commands of
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
executeAndMonitorCommand Params{..} MonitorOptions{..} command
  | monitor = do
        getDuration <- startDuration
        pid <- Posix.forkProcess (executeCommand command)
        exitCode <- getProcessStatus pid
        duration <- getDuration
        message defaultLayoutOptions verbosity colour stdout
            ( "Action took: " <> Pretty.viaShow duration <> Pretty.line
                :: Pretty.Doc (Result Pretty.AnsiStyle)
            )
        -- TODO: Value of type `NotifyConfig` should be part of `Config`.
        doNotifyWhen defNotifyConfig True notificationMessage exitCode
        exitWith exitCode

  | otherwise =
        executeCommand command
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
    return $ do
        end <- getTime
        pure (end - start)
  where
    getTime = Clock.getTime Clock.Monotonic

executeCommand :: Command -> IO ()
executeCommand Command{..} = do
    for_ workingDirectory setCurrentDirectory
    mkEnv environment >>= Posix.executeFile command searchPath arguments . Just
  where
    mkEnv :: [EnvironmentVariable] -> IO [(String, String)]
    mkEnv additionalVars = getEnvironment <&> \env ->
        Map.toList (removeCommandwrapperVars env <> additionalVars')
      where
        additionalVars' =
            Map.fromList (varToTuple <$> additionalVars)

        removeCommandwrapperVars env =
            foldMap (Endo . Map.delete) commandWrapperVars
                `appEndo` Map.fromList env
          where
            commandWrapperVars =
                List.filter ("COMMAND_WRAPPER_" `List.isPrefixOf`) (fst <$> env)

        varToTuple =
            bimap Text.unpack Text.unpack . EnvironmentVariable.toTuple

printAsDhall :: Dhall.Inject a => Params -> a -> IO ()
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
    mkCommand <- Dhall.input Dhall.auto expression
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

data TreeOptions = TreeOptions

data TreeAnn = NodeName | LeafName

showCommandTree :: Params -> Config -> TreeOptions -> IO ()
showCommandTree Params{colour} Config{commands} TreeOptions{} =
    putDoc (treeDoc <> Pretty.line)
  where
    names :: [[Text]] =
        Text.split (== '.') . (name :: NamedCommand -> Text) <$> commands

    groupNames :: [[Text]] -> [[[Text]]] =
        List.groupBy ((==) `on` headMay) . List.sortBy (compare `on` headMay)

    forest :: Forest (Doc TreeAnn) = (`unfoldForest` groupNames names) \case
        (n : r) : ns ->
            let rs = groupNames $ r : fmap (drop 1) ns
                isLeaf = [[]] `elem` rs
                ann = Pretty.annotate (if isLeaf then LeafName else NodeName)

             in ( ann (pretty n) <> if isLeaf then "*" else ""
                , List.filter (/= [[]]) rs
                )

        _ -> ("", []) -- This should not happen.

    draw :: Tree (Doc ann) -> [Doc ann]
    draw (Node x ts0) = x : drawSubTrees ts0
      where
        drawSubTrees = \case
            [] ->
                []
            [t] ->
                shift "└── " "    " (draw t)
            t : ts ->
                shift "├── " "│   " (draw t) <> drawSubTrees ts

        shift first other = zipWith (<>) (first : repeat other)

    treeDoc = Pretty.vsep $ drop 1 (draw (Node "" forest))

    putDoc = hPutDoc defaultLayoutOptions toAnsi colour stdout

    toAnsi = \case
        LeafName -> Pretty.color Pretty.Magenta
        NodeName -> Pretty.color Pretty.Blue

parseOptions :: Options.Parser (Action -> Action)
parseOptions = asum
    [ completionInfoFlag
    , Options.flag' (const (Run True)) (Options.long "notify")
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

    , Options.flag' (const Help) (Options.short 'h' <> Options.long "help")

    -- Command line completion:
    , const
        <$> ( Options.flag' Completion
                (Options.long "completion" <> Options.internal)
            <*> indexOption
            <*> shellOption
            )

    , pure id
    ]
  where
    expressionOption =
        Options.strOption (Options.long "expression") <&> \expr m _ ->
            RunDhall m expr

    indexOption :: Options.Parser Word
    indexOption =
        Options.option Options.auto (Options.long "index" <> Options.internal)

    shellOption :: Options.Parser Options.Shell
    shellOption = Options.option
        (Options.maybeReader (Options.Shell.parse . CaseInsensitive.mk))
        (Options.long "shell" <> Options.internal)

doCompletion :: Params -> Config -> Options.Shell -> Word -> [String] -> IO ()
doCompletion params config@Config{commands} shell index words =
    case Options.splitArguments' words of
        (_, _, []) ->
            completeExecSubcommand

        (_, indexBound, commandName : commandArguments)
          | not canCompleteCommand || index <= indexBound ->
                completeExecSubcommand
          | otherwise ->
                doCommandCompletion params config shell commandName
                    (index - indexBound) commandArguments
  where
    completeExecSubcommand
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
    -> Options.Shell
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
            executeCommandCompletion mkCompletionCommand shell index words
  where
    -- TODO: Should this be the same default completion as shell does?
    defaultCompletion = pure ()

getCompletion
    :: Params
    -> Config
    -> String
    -> IO (Maybe (Options.Shell -> Natural -> [Text] -> Command))
getCompletion params Config{commands} commandName =
    case List.find (NamedCommand.isNamed (fromString commandName)) commands of
        Nothing ->
            dieWith params stderr 1
                ("'" <> fromString commandName <> "': Missing argument.")

        Just NamedCommand{completion} ->
            pure completion

executeCommandCompletion
    :: (Options.Shell -> Natural -> [Text] -> Command)
    -- ^ Function that constructs completion command, i.e. command that will be
    -- executed to provide completion.
    -> Options.Shell
    -> Word
    -> [String]
    -> IO ()
executeCommandCompletion mkCommand shell index words =
    executeCommand (mkCommand shell (fromIntegral index) (fromString <$> words))

helpMsg :: Params -> Pretty.Doc (Result Pretty.AnsiStyle)
helpMsg Params{name, subcommand} = Pretty.vsep
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
        ]

    , Help.section (Help.metavar "COMMAND")
        [ Help.metavar "COMMAND" <+> Pretty.reflow "to execute."
        ]
    , ""

    , Help.section (Help.metavar "COMMAND_ARGUMENTS")
        [ Pretty.reflow "Additional arguments passed to"
        <+> Help.metavar "COMMAND" <> "."
        ]
    , ""
    ]
  where
    subcommand' = fromString subcommand
