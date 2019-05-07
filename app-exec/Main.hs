{-# LANGUAGE BlockArguments #-}
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

import Control.Applicative (many)
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
import qualified Data.Text.IO as Text (putStrLn)
import Data.Tree (Forest, Tree(Node), drawTree, unfoldForest)
import qualified Dhall (Interpret, auto, inject, inputFile)
import qualified Dhall.Pretty as Dhall (CharacterSet(Unicode))
import qualified Options.Applicative as Options
    ( Parser
    , auto
    , defaultPrefs
    , execParserPure
    , flag'
    , fullDesc
    , handleParseResult
    , help
    , helper
    , info
    , internal
    , long
    , metavar
    , option
    , progDesc
    , short
    , strArgument
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
import qualified CommandWrapper.Options as Options (splitArguments)
import qualified CommandWrapper.Internal.Dhall as Dhall (hPut)
import CommandWrapper.Prelude
    ( HaveCompletionInfo(completionInfoMode)
    , Params(Params, colour, config, verbosity)
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

instance HaveCompletionInfo Action where
    completionInfoMode = const CompletionInfo

main :: IO ()
main = do
    params@Params{config = configFile} <- subcommandParams

    (options, commandAndItsArguments) <- Options.splitArguments <$> getArgs

    Config{commands} <- Dhall.inputFile Dhall.auto configFile
    action <- fmap ($ Run) . Options.handleParseResult
        $ Options.execParserPure
            Options.defaultPrefs
            (Options.info (Options.helper <*> parseOptions) description)
            options

    case action of
        List ->
            mapM_ (Text.putStrLn . (name :: NamedCommand -> Text)) commands

        Tree ->
            let names :: [[Text]] =
                    Text.split (== '.') . (name :: NamedCommand -> Text)
                    <$> commands

                groupNames :: [[Text]] -> [[[Text]]] =
                    List.groupBy ((==) `on` headMay)
                    . List.sortBy (compare `on` headMay)

                forest :: Forest Text = (`unfoldForest` groupNames names) \case
                    (n : r) : ns ->
                        let rs = groupNames $ r : fmap (drop 1) ns
                         in ( n <> if [[]] `elem` rs then "*" else ""
                            , List.filter (/= [[]]) rs
                            )

                    _ -> ("", []) -- This should not happen.

             in putStrLn $ drawTree (Text.unpack <$> Node "" forest)

        DryRun ->
            getExecutableCommand params commands commandAndItsArguments
                >>= printCommand params

        Run ->
            getExecutableCommand params commands commandAndItsArguments
                >>= executeCommand

        CompletionInfo ->
            printCommandWrapperStyleCompletionInfoExpression stdout

        Completion index ->
            let pat =
                    fromMaybe (lastDef "" commandAndItsArguments)
                        (atMay commandAndItsArguments (fromIntegral index))

                commandNames =
                    Text.unpack . (name :: NamedCommand -> Text) <$> commands

                allOptions =
                    [ "-l", "--ls", "--list"
                    , "-t", "--tree"
                    , "--print"
                    , "-h", "--help"
                    ]

             in mapM_ putStrLn . List.filter (pat `List.isPrefixOf`)
                    $ allOptions <> commandNames
  where
    getExecutableCommand params commands commandAndItsArguments =
        case fromString <$> commandAndItsArguments of
            [] ->
                dieWith params stderr 1 "COMMAND: Missing argument."

            name : arguments ->
                getCommand params commands name arguments

    description = mconcat
        [ Options.fullDesc
        , Options.progDesc "Execute a command with predefined environment and\
            \ command line options."
        ]

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

parseOptions :: Options.Parser (Action -> Action)
parseOptions = asum
    [ completionInfoFlag
    , Options.flag' (const List) $ mconcat
        [ Options.short 'l'
        , Options.long "list"
        , Options.long "ls"
        , Options.help "List available COMMANDs."
        ]
    , Options.flag' (const Tree) $ mconcat
        [ Options.long "tree"
        , Options.short 't'
        , Options.help
            "List available COMMANDs in tree-like form treating dots (`.`) as\
            \ separators."
        ]
    , Options.flag' (const DryRun) $ mconcat
        [ Options.long "print"
        , Options.help "Print command as it will be executed in Dhall format."
        ]

    -- Command line completion.
    , const
        <$> ( Options.flag' Completion
                (Options.long "completion" <> Options.internal)
            <*> Options.option Options.auto
                (Options.long "index" <> Options.internal)
            <*  Options.strOption @String
                (Options.long "shell" <> Options.internal)
            )

    -- This is here purely to provide better help message.
    , (\(_ :: String) (_ :: [String]) -> id :: Action -> Action)
        <$> ( Options.strArgument $ mconcat
                [ Options.metavar "COMMAND"
                , Options.help "COMMAND to execute."
                ]
            )
        <*> many
            ( Options.strArgument $ mconcat
                [ Options.metavar "EXTRA_COMMAND_ARGUMENT"
                , Options.help "Additional arguments passed to COMMAND."
                ]
            )

    , pure id
    ]

-- TODO:
--
-- *  Allow alternatives, i.e. have a list of commands for one `NAME` and the
--    first one that is available is used.  (Just an idea.)
--
-- *  Implement command-line completion, when available in `command-wrapper`
--    itself.
--
-- *  Evaluate command with and without extra arguments.  If the result is the
--    same then print warning to the user.  Dual case would be interesting as
--    well.  Having the ability to tell when the command requires additional
--    arguments, but there is no obvious simple solution.
--
-- *  Ability to take Dhall function of the following type:
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
-- *  Support desktop notifications:
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
-- * Nicer `--tree` representation that uses colours to make the distinction
--   between executable commands and purely organisation nodes more obvious.
