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

import Data.Functor ((<&>))
import qualified Data.List as List (filter, find, isPrefixOf)
import Data.Monoid (Endo(..))
import Data.String (fromString)
import GHC.Generics (Generic)
import System.Environment (getArgs, getEnvironment)
import System.Exit (die)

import qualified Data.Map.Strict as Map (delete, fromList, toList)
import Data.Text (Text)
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
import qualified System.Posix as Posix (executeFile)

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
    -- ^ List of variables to be added to current environment before executing
    -- command.
    , searchPath :: Bool
    -- ^ Search @PATH@ when looking for 'command'?
    }
  deriving (Generic, Show)

instance Dhall.Interpret Command

main :: IO ()
main = do
    Environment.Params{config = configFile, verbosity} <- parseEnv

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
                    getCommand commands name verbosity arguments
                    >>= executeCommand
  where
    description = mconcat
        [ Options.fullDesc
        , Options.progDesc "Execute a command with predefined environment and\
            \command line options"
        ]

getCommand :: [NamedCommand] -> Text -> Verbosity -> [Text] -> IO Command
getCommand commands expectedName verbosity arguments =
    case List.find (\NamedCommand{name} -> name == expectedName) commands of
        Nothing ->
            die (show expectedName <> ": Unknown COMMAND.")

        Just NamedCommand{command} ->
            pure (command verbosity arguments)

executeCommand :: Command -> IO ()
executeCommand Command{..} =
    mkEnv environment >>= Posix.executeFile command searchPath arguments . Just
  where
    mkEnv :: [EnvironmentVariable] -> IO [(String, String)]
    mkEnv additionalVars = getEnvironment <&> \env ->
        Map.toList (removeCommandwrapperVars env <> additionalVars')
      where
        additionalVars' =
            Map.fromList
                $ additionalVars <&> \EnvironmentVariable{name, value} ->
                    (name, value)

        removeCommandwrapperVars env =
            foldMap (Endo . Map.delete) commandWrapperVars
                `appEndo` Map.fromList env
          where
            commandWrapperVars =
                List.filter ("COMMAND_WRAPPER_" `List.isPrefixOf`) (fst <$> env)

parseEnv :: IO Environment.Params
parseEnv = Environment.parseEnvIO (die . show) Environment.askParams

parseOptions :: Options.Parser (Bool -> Bool)
parseOptions =
    Options.flag' (const True) (Options.short 'l' <> Options.long "ls")

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
