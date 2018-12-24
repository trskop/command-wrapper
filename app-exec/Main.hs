{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module:      Main
-- Description: CommandWrapper subcommand for executing commands with a
--              predefined environment.
-- Copyright:   (c) 2018 Peter Trško
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

import Control.Applicative ((<|>))
import Data.Bifunctor (bimap)
import Data.Foldable (for_)
import Data.Functor ((<&>))
import qualified Data.List as List (filter, find, isPrefixOf)
import Data.Monoid (Endo(..))
import Data.String (fromString)
import GHC.Generics (Generic)
import System.Environment (getArgs, getEnvironment)
import System.Exit (die)

import qualified Data.Map.Strict as Map (delete, fromList, toList)
import Data.Text (Text)
import qualified Data.Text as Text (unpack)
import qualified Data.Text.IO as Text (putStrLn)
import qualified Dhall (Interpret, auto, inputFile)
import Data.Verbosity (Verbosity)
import qualified Options.Applicative as Options
    ( Parser
    , defaultPrefs
    , execParserPure
    , flag'
    , fullDesc
    , handleParseResult
    , helper
    , info
    , long
    , progDesc
    , short
    )
import System.Directory (setCurrentDirectory)
import qualified System.Posix as Posix (executeFile)

import CommandWrapper.Config.Command (Command(..), NamedCommand(..))
import qualified CommandWrapper.Config.Command as NamedCommand (isNamed)
import CommandWrapper.Config.Environment (EnvironmentVariable(..))
import qualified CommandWrapper.Config.Environment as EnvironmentVariable
    ( toTuple
    )
import qualified CommandWrapper.Environment as Environment
import qualified CommandWrapper.Options as Options (splitArguments)
import CommandWrapper.Options.ColourOutput (ColourOutput)


newtype Config = Config
    { commands :: [NamedCommand]
    }
  deriving (Generic)

instance Dhall.Interpret Config

main :: IO ()
main = do
    Environment.Params{config = configFile, verbosity, colour} <- parseEnv

    (options, commandAndItsArguments) <- Options.splitArguments <$> getArgs

    Config{commands} <- Dhall.inputFile Dhall.auto configFile
    listCommands <- fmap ($ False) . Options.handleParseResult
        $ Options.execParserPure
            Options.defaultPrefs
            (Options.info (Options.helper <*> parseOptions) description)
            options

    if listCommands
        then
            mapM_ (Text.putStrLn . (name :: NamedCommand -> Text)) commands
        else
            case fromString <$> commandAndItsArguments of
                [] ->
                    die "COMMAND: Missing argument."

                name : arguments ->
                    getCommand commands name verbosity colour arguments
                    >>= executeCommand
  where
    description = mconcat
        [ Options.fullDesc
        , Options.progDesc "Execute a command with predefined environment and\
            \command line options"
        ]

getCommand
    :: [NamedCommand]
    -> Text
    -> Verbosity
    -> ColourOutput
    -> [Text]
    -> IO Command
getCommand commands expectedName verbosity colourOutput arguments =
    case List.find (NamedCommand.isNamed expectedName) commands of
        Nothing ->
            die (show expectedName <> ": Unknown COMMAND.")

        Just NamedCommand{command} ->
            pure (command verbosity colourOutput arguments)

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

parseEnv :: IO Environment.Params
parseEnv = Environment.parseEnvIO (die . show) Environment.askParams

parseOptions :: Options.Parser (Bool -> Bool)
parseOptions =
    Options.flag' (const True) (Options.short 'l' <> Options.long "ls")
    <|> pure id

-- TODO:
--
-- * Allow alternatives, i.e. have a list of commands for one `NAME` and the
--   first one that is available is used.  (Just an idea.)
--
-- * Implement command-line completion, when available in `command-wrapper`
--   itself.
--
-- * Evaluate command with and without extra arguments.  If the result is the
--   same then print warning to the user.  Dual case would be interesting as
--   well.  Having the ability to tell when the command requires additional
--   arguments, but there is no obvious simple solution.
