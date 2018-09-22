{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
--{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
--{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
--{-# LANGUAGE TypeApplications #-}
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

import qualified Data.List as List (find)
import Data.String (fromString)
import GHC.Generics (Generic)
import System.Environment (getArgs)
import System.Exit (die)

import Data.Text (Text)
import qualified Dhall (Interpret, auto, inputFile)
import Data.Verbosity (Verbosity)
import qualified Options.Applicative as Options
    ( defaultPrefs
    , execParserPure
    , fullDesc
    , handleParseResult
    , helper
    , info
    , progDesc
    )
import qualified Turtle

import qualified CommandWrapper.Environment as Environment
import qualified CommandWrapper.Options as Options (splitArguments)


newtype Config = Config
    { commands :: [NamedCommand]
    }
  deriving (Generic)

instance Dhall.Interpret Config

data NamedCommand = NamedCommand
    { name :: Text
    , command :: Verbosity -> [Text] -> Command
    }
  deriving (Generic)

instance Dhall.Interpret NamedCommand

data EnvironmentVariable = EnvironmentVariable
    { name :: String
    , value :: String
    }
  deriving (Generic, Show)

instance Dhall.Interpret EnvironmentVariable

data Command = Command
    { command :: FilePath
    , arguments :: [String]
    , environment :: [EnvironmentVariable]
    }
  deriving (Generic, Show)

instance Dhall.Interpret Command

main :: IO ()
main = do
    Environment.Params{config = configFile, verbosity} <- parseEnv

    (options, commandAndItsArguments) <- Options.splitArguments <$> getArgs

    Config{commands} <- Dhall.inputFile Dhall.auto configFile
    Options.handleParseResult
        $ Options.execParserPure
            Options.defaultPrefs
            (Options.info (Options.helper <*> parseOptions) description)
            options

    case fromString <$> commandAndItsArguments of
        [] ->
            die "COMMAND: Missing argument."

        name : arguments ->
            executeCommand commands name verbosity arguments
  where
    description = mconcat
        [ Options.fullDesc
        , Options.progDesc "Execute a command with predefined environment and\
            \command line options"
        ]

executeCommand :: [NamedCommand] -> Text -> Verbosity -> [Text] -> IO ()
executeCommand commands expectedName verbosity arguments =
    getCommand >>= print
  where
    getCommand :: IO Command
    getCommand =
        case List.find (\NamedCommand{name} -> name == expectedName) commands of
            Nothing ->
                die (show expectedName <> ": Unknown COMMAND.")

            Just NamedCommand{command} ->
                pure (command verbosity arguments)

parseEnv :: IO Environment.Params
parseEnv = Environment.parseEnvIO (die . show) Environment.askParams

parseOptions :: Turtle.Parser ()
parseOptions = pure ()

-- TODO:
--
--   * Remove COMMAND_WRAPPER_* variables from environment before executing the
--     command.

-- Usage:
--
--   TOOLSET exec NAME [ARGUMENTS]
--
-- Examples:
--
--   TOOLSET exec psql-production
