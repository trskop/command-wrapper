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

import qualified Data.Map.Strict as Map (delete, fromList, toList)
import Data.Text (Text)
import qualified Data.Text as Text (unpack)
import qualified Data.Text.IO as Text (putStrLn)
import qualified Dhall (Interpret, auto, inputFile)
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
import qualified CommandWrapper.Options as Options (splitArguments)
import CommandWrapper.Prelude
    ( Params(Params, colour, config, verbosity)
    , dieWith
    , stderr
    , subcommandParams
    )


newtype Config = Config
    { commands :: [NamedCommand]
    }
  deriving (Generic)

instance Dhall.Interpret Config

main :: IO ()
main = do
    params@Params{config = configFile} <- subcommandParams

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
                    dieWith params stderr 1 "COMMAND: Missing argument."

                name : arguments ->
                    getCommand params commands name arguments
                    >>= executeCommand
  where
    description = mconcat
        [ Options.fullDesc
        , Options.progDesc "Execute a command with predefined environment and\
            \command line options"
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
