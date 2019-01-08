{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:      CommandWrapper.Options.Optparse
-- Description: Utilities for parsing command line options.
-- Copyright:   (c) 2018-2019 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Utilities for parsing command line options.
module CommandWrapper.Options.Optparse
    (
    -- * Generic API
      parse

    -- ** Helper Functions
    , splitArguments
    , execParserPure
    , handleParseResult
    )
  where

import Control.Applicative (pure)
import Data.Bool ((&&))
import Data.Either (Either(Left, Right))
import Data.Eq ((/=), (==))
import Data.Functor (Functor, (<$>))
import Data.Function (($))
import qualified Data.List as List (span)
import Data.Maybe (listToMaybe)
import Data.Monoid (Endo, (<>))
import Data.String (String)
import System.Environment (getArgs)
import System.Exit (ExitCode(ExitFailure), exitWith)
import System.IO (IO, stderr)

import Control.Comonad (Comonad, extract)
import qualified Data.Verbosity as Verbosity (Verbosity(Normal))
import qualified Data.Output.Colour as ColourOutput (ColourOutput(Auto))
import qualified Options.Applicative as Options
    ( ParserFailure(ParserFailure, execFailure)
    , ParserInfo
    , ParserPrefs
    , ParserResult(CompletionInvoked, Failure, Success)
    , parserFailure
    )
import qualified Options.Applicative.Common as Options (runParserInfo)
import qualified Options.Applicative.Internal as Options (runP)

import CommandWrapper.Environment.AppNames (AppNames(AppNames, usedName))
import CommandWrapper.Message (dieFailedToParseOptions)


parse
    :: forall globalMode mode config
    .  (Functor mode, Comonad globalMode)
    => AppNames
    -> Options.ParserPrefs
    -> Options.ParserInfo (globalMode (Endo config))
    -> (forall a. globalMode (Endo a) -> Endo (mode a))
    -> (forall a. Endo config -> [String] -> IO (Endo (mode a)))
    -> IO (Endo (mode config))
parse appNames parserPrefs parserInfo fromGlobalMode parseArguments = do
    (globalOptions, arguments) <- splitArguments <$> getArgs

    globalMode <- handleParseResult appNames (execParserPure' globalOptions)

    let updateConfig = extract globalMode
    updateCommand <- parseArguments updateConfig arguments

    pure $ fromGlobalMode globalMode <> updateCommand
  where
    execParserPure'
        :: [String]
        -> Options.ParserResult (globalMode (Endo config))
    execParserPure' = execParserPure parserPrefs parserInfo

-- | Split arguments into global options and the rest.
--
-- @
-- COMMAND_WRAPPER [GLOBAL_OPTIONS] [[--] SUBCOMMAND [SUBCOMMAND_ARGUMENTS]]
-- @
--
-- >>> splitArguments ["-i", "--help"]
-- (["-i", "--help"], [])
-- >>> splitArguments ["-i", "help", "build"]
-- (["-i"], ["help", "build"])
-- >>> splitArguments ["-i", "--", "help", "build"]
-- (["-i"], ["help", "build"])
-- >>> splitArguments ["-i", "--", "--foo"]
-- (["-i"], ["--foo"])
splitArguments
    :: [String]
    -> ([String], [String])
splitArguments args =
    case List.span (\arg -> arg /= "--" && listToMaybe arg == pure '-') args of
        (globalOptions, "--" : subcommandAndItsArguments) ->
            (globalOptions, subcommandAndItsArguments)

        args' -> args'

-- | Variant of 'Options.Applicative.execParserPure' that doesn't provide shell
-- completion.
execParserPure
    :: Options.ParserPrefs
    -- ^ Global preferences for this parser
    -> Options.ParserInfo a
    -- ^ Description of the program to run
    -> [String]
    -- ^ Program arguments
    -> Options.ParserResult a
execParserPure pprefs pinfo args =
  case Options.runP (Options.runParserInfo pinfo args) pprefs of
    (Right r, _) ->
        Options.Success r

    (Left err, ctx) ->
        Options.Failure (Options.parserFailure pprefs pinfo err ctx)

handleParseResult :: AppNames -> Options.ParserResult a -> IO a
handleParseResult AppNames{usedName} = \case
    Options.Success a ->
        pure a

    Options.Failure Options.ParserFailure{Options.execFailure} ->
        let (err, _, _) = execFailure usedName
        in dieFailedToParseOptions usedName Verbosity.Normal ColourOutput.Auto
            stderr err

    Options.CompletionInvoked _ ->
        exitWith (ExitFailure 1) -- TODO: This is imposible case.
