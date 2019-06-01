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

import Data.Bifunctor (bimap)
import Data.Foldable (asum, elem, for_)
import Data.Function (on)
import Data.Functor ((<&>))
import qualified Data.List as List (filter, find, groupBy, isPrefixOf, sortBy)
import Data.Maybe (fromMaybe)
import Data.Monoid (Endo(..))
import Data.String (fromString)
import GHC.Generics (Generic)
import System.Environment (getArgs, getEnvironment)

import qualified Data.Map.Strict as Map (delete, fromList, toList)
import Data.Text (Text)
import qualified Data.Text as Text (split, unpack)
--import qualified Data.Text.IO as Text (putStrLn)
import Data.Text.Prettyprint.Doc (Doc, (<+>), pretty)
import qualified Data.Text.Prettyprint.Doc as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Pretty
    ( AnsiStyle
    , Color(Magenta, White)
    , color
    , colorDull
    )
import qualified Data.Text.Prettyprint.Doc.Util as Pretty (reflow)
import Data.Tree (Forest, Tree(Node), unfoldForest)
import qualified Dhall (Interpret, auto, inject, inputFile)
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
    )
import Safe (atMay, headMay, lastDef)
import System.Directory (setCurrentDirectory)
import qualified System.Posix as Posix (executeFile)

import CommandWrapper.Config.Command (Command(..), NamedCommand(..))
import qualified CommandWrapper.Config.Command as NamedCommand (isNamed)
import CommandWrapper.Config.Environment (EnvironmentVariable(..))
import qualified CommandWrapper.Config.Environment as EnvironmentVariable
    ( toTuple
    )
import qualified CommandWrapper.Internal.Dhall as Dhall (hPut)
import qualified CommandWrapper.Internal.Subcommand.Help as Help
import CommandWrapper.Message (Result, defaultLayoutOptions, hPutDoc, message)
import qualified CommandWrapper.Options as Options (splitArguments)
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
    }
  deriving stock (Generic)
  deriving anyclass (Dhall.Interpret)

data Action
    = List
    | Tree
    | DryRun
    | Run
    | CompletionInfo
    | Completion Word
    | Help

instance HaveCompletionInfo Action where
    completionInfoMode = const CompletionInfo

main :: IO ()
main = do
    params@Params{config = configFile} <- subcommandParams

    (options, commandAndItsArguments) <- Options.splitArguments <$> getArgs

    config@Config{commands} <- Dhall.inputFile Dhall.auto configFile
    action <- fmap ($ Run) . Options.handleParseResult
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
            getExecutableCommand params commands commandAndItsArguments
                >>= printCommand params

        Run ->
            getExecutableCommand params commands commandAndItsArguments
                >>= executeCommand

        CompletionInfo ->
            printCommandWrapperStyleCompletionInfoExpression stdout

        Completion index ->
            mapM_ putStrLn (doCompletion config index commandAndItsArguments)

        Help ->
            let Params{verbosity, colour} = params
             in message defaultLayoutOptions verbosity colour stdout
                    (helpMsg params)
  where
    getExecutableCommand params commands commandAndItsArguments =
        case fromString <$> commandAndItsArguments of
            [] ->
                dieWith params stderr 1 "COMMAND: Missing argument."

            name : arguments ->
                getCommand params commands name arguments

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

printCommand :: Params -> Command -> IO ()
printCommand Params{colour} =
    Dhall.hPut colour Dhall.Unicode stdout Dhall.inject

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

showCommandTree :: Params -> Config -> TreeOptions -> IO ()
showCommandTree Params{} Config{commands} TreeOptions{} =
    putStrLn . unlines $ draw (Text.unpack <$> Node "" forest)
  where
    names :: [[Text]] =
        Text.split (== '.') . (name :: NamedCommand -> Text) <$> commands

    groupNames :: [[Text]] -> [[[Text]]] =
        List.groupBy ((==) `on` headMay) . List.sortBy (compare `on` headMay)

    forest :: Forest Text = (`unfoldForest` groupNames names) \case
        (n : r) : ns ->
            let rs = groupNames $ r : fmap (drop 1) ns
             in ( n <> if [[]] `elem` rs then "*" else ""
                , List.filter (/= [[]]) rs
                )

        _ -> ("", []) -- This should not happen.

    draw :: Tree String -> [String]
    draw (Node x ts0) = lines x <> drawSubTrees ts0
      where
        drawSubTrees = \case
            [] ->
                []
            [t] ->
                shift "└── " "    " (draw t)
            t : ts ->
                shift "├── " "│   " (draw t) <> drawSubTrees ts

        shift first other = zipWith (<>) (first : repeat other)


parseOptions :: Options.Parser (Action -> Action)
parseOptions = asum
    [ completionInfoFlag
    , Options.flag' (const List) $ mconcat
        [ Options.short 'l'
        , Options.long "list"
        , Options.long "ls"
        ]
    , Options.flag' (const Tree) (Options.long "tree" <> Options.short 't')
    , Options.flag' (const DryRun) (Options.long "print")
    , Options.flag' (const Help) (Options.short 'h' <> Options.long "help")

    -- Command line completion:
    , const
        <$> ( Options.flag' Completion
                (Options.long "completion" <> Options.internal)
            <*> Options.option Options.auto
                (Options.long "index" <> Options.internal)
            <*  Options.strOption @String
                (Options.long "shell" <> Options.internal)
            )

    , pure id
    ]

doCompletion :: Config -> Word -> [String] -> [String]
doCompletion Config{commands} index words =
    List.filter (pat `List.isPrefixOf`) $ allOptions <> commandNames
  where
    pat = fromMaybe (lastDef "" words) (atMay words (fromIntegral index))

    commandNames = Text.unpack . (name :: NamedCommand -> Text) <$> commands

    allOptions =
        [ "-l", "--ls", "--list"
        , "-t", "--tree"
        , "--print"
        , "-h", "--help"
        ]

helpMsg :: Params -> Pretty.Doc (Result Pretty.AnsiStyle)
helpMsg Params{name, subcommand} = Pretty.vsep
    [ Pretty.reflow "Execute a command with predefined environment and command\
        \ line options."
    , ""

    , Help.usageSection name
        [ subcommand'
            <+> Pretty.brackets (Help.longOption "print")
            <+> Pretty.brackets (Help.metavar "--")
            <+> Help.metavar "COMMAND"
            <+> Pretty.brackets (Help.metavar "EXTRA_COMMAND_ARGUMENT")

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

    , Help.section (Help.metavar "EXTRA_COMMAND_ARGUMENT")
        [ Pretty.reflow "Additional arguments passed to"
        <+> Help.metavar "COMMAND" <> "."
        ]
    , ""
    ]
  where
    subcommand' = fromString subcommand

-- TODO:
--
-- -  Allow alternatives, i.e. have a list of commands for one `NAME` and the
--    first one that is available is used.  (Just an idea.)
--
-- -  Evaluate command with and without extra arguments.  If the result is the
--    same then print warning to the user.  Dual case would be interesting as
--    well.  Having the ability to tell when the command requires additional
--    arguments, but there is no obvious simple solution.
--
-- -  Ability to take Dhall function of the following type:
--
--    ```
--    Verbosity -> ColourOutput -> [Text] -> Command
--    ```
--
--    As an argument and execute it.
--
--    ```
--    TOOLSET exec --command=DHALL [ARGUMENTS]
--    ```
--
--    Since Dhall supports imports, the above will work for files and URLs as
--    well.
--
-- -  Support desktop notifications:
--
--    ```
--    { commands =
--        [ ...
--        , { name = "build-and-notify"
--          , command = ./build-command.dhall
--          , notify =
--              Some
--                { when = <Always = {=} | TakesLongerThan : Natural>
--                , params =
--                      \(exitCode : <ExitSuccess : {} | ExitFailure : Natural>)
--                    -> { icon = foldExitCode (Optional Text) (Some "dialog-information") (Some "dialog-error") exitCode
--                       , urgency = foldExitCode (Optional Text) (Some "low") (Some "normal") exitCode
--                       , message = "Build ${foldExitCode Text "finished successfully" "failed"}."
--                       , soundFile = None Text
--                       }
--                }
--          }
--        , ...
--        ]
--    }
--    ```
--
-- -  Nicer `--tree` representation that uses colours to make the distinction
--    between executable commands and purely organisation nodes more obvious.
