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

import Control.Applicative ((<|>))
import Data.Functor (void)
import GHC.Generics (Generic)
import System.Exit (die)

import Data.Text (Text)
import qualified Dhall (Interpret, auto, inputFile)
import Data.Verbosity (Verbosity)
import qualified Data.Verbosity as Verbosity (Verbosity(Normal))
import qualified Turtle

import qualified CommandWrapper.Environment as Environment


newtype Config = Config
    { commands :: Verbosity -> [NamedCommand]
    }
  deriving (Generic)

instance Dhall.Interpret Config

data NamedCommand = NamedCommand
    { name :: Text
    , command :: Command
    }
  deriving (Generic, Show)

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
    (configFile, _verbosity) <- Environment.parseEnvIO (die . show) $ do
        void (Environment.askVar "COMMAND_WRAPPER_EXE")
            <|> failInvalidCommandWrapperEnvironment

        void (Environment.askVar "COMMAND_WRAPPER_NAME")
            <|> failInvalidCommandWrapperEnvironment

        (,) <$> Environment.askVar "COMMAND_WRAPPER_CONFIG"
            <*> Environment.askVar "COMMAND_WRAPPER_VERBOSITY"

    Config{commands} <- Dhall.inputFile Dhall.auto configFile
    () <- Turtle.options description parseOptions

    print (commands Verbosity.Normal)
  where
    description =
        "Execute a command with predefined environment and command line options"

    failInvalidCommandWrapperEnvironment =
        fail "This command must be executed as part of some command-wrapper environment"

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
