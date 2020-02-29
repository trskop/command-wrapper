-- |
-- Module:      $Header$
-- Description: Utilities for parsing command line options.
-- Copyright:   (c) 2018-2020 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Utilities for parsing command line options.
module CommandWrapper.Core.Options.Optparse
    (
    -- * Generic API
      parse
    , subcommandParse

    -- ** Helper Functions
    , splitArguments
    , splitArguments'
    , execParserPure
    , handleParseResult
    )
  where

import Prelude ((+), fromIntegral)

import Control.Applicative (pure)
import Data.Bool ((&&))
import Data.Either (Either(Left, Right))
import Data.Eq ((/=), (==))
import Data.Foldable (length)
import Data.Function (($), (.))
import Data.Functor (Functor, (<$>))
import qualified Data.List as List (span)
import Data.Maybe (listToMaybe)
import Data.Monoid (Endo, (<>))
import Data.String (String)
import Data.Word (Word)
import System.Environment (getArgs)
import System.Exit (ExitCode(ExitFailure), exitWith)
import System.IO (IO, stderr)

import Control.Comonad (Comonad, extract)
import Data.Verbosity (Verbosity)
import qualified Data.Verbosity as Verbosity (Verbosity(Normal))
import Data.Output.Colour (ColourOutput)
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

import CommandWrapper.Core.Environment.AppNames (AppNames(AppNames, usedName))
import CommandWrapper.Core.Environment.Params
    ( Params(Params, colour, name, subcommand, verbosity)
    )
import CommandWrapper.Core.Message (dieFailedToParseOptions)


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

    globalMode <- handleParseResult' appNames (execParserPure' globalOptions)

    let updateConfig = extract globalMode
    updateCommand <- parseArguments updateConfig arguments

    pure $ fromGlobalMode globalMode <> updateCommand
  where
    execParserPure'
        :: [String]
        -> Options.ParserResult (globalMode (Endo config))
    execParserPure' = execParserPure parserPrefs parserInfo

    handleParseResult' AppNames{usedName} =
        -- TODO: We should at least respect `NO_COLOR`.
        handleParseResult usedName Verbosity.Normal ColourOutput.Auto

subcommandParse
    :: Params
    -> Options.ParserPrefs
    -> Options.ParserInfo (Endo (mode config))
    -> [String]
    -- ^ Command line arguments.  Usually obtained by 'getArgs'.
    -> IO (Endo (mode config))
subcommandParse params parserPrefs parserInfo =
    handleParseResult' params . execParserPure'
  where
    execParserPure' = execParserPure parserPrefs parserInfo

    handleParseResult' Params{colour, name, subcommand, verbosity} =
        handleParseResult (name <> " " <> subcommand) verbosity colour

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
splitArguments args = (globalOptions, subcommandAndItsArguments)
  where
    (globalOptions, _, subcommandAndItsArguments) = splitArguments' args

-- | Split arguments into global options and the rest.
--
-- @
-- COMMAND_WRAPPER [GLOBAL_OPTIONS] [[--] SUBCOMMAND [SUBCOMMAND_ARGUMENTS]]
-- @
--
-- >>> splitArguments' ["-i", "--help"]
-- (["-i", "--help"], 2, [])
-- >>> splitArguments' ["-i", "help", "build"]
-- (["-i"], 1, ["help", "build"])
-- >>> splitArguments' ["-i", "--", "help", "build"]
-- (["-i"], 2, ["help", "build"])
-- >>> splitArguments' ["-i", "--", "--foo"]
-- (["-i"], 2, ["--foo"])
--
-- >>> let arguments = ["-i", "--", "help", "build"]
-- >>> let (_, n, subcommandAndItsArguments) = splitArguments' arguments
-- >>> drop n arguments == subcommandAndItsArguments
-- True
splitArguments'
    :: [String]
    -> ([String], Word, [String])
splitArguments' args = case subcommandAndItsArguments' of
    "--" : subcommandAndItsArguments ->
        (globalOptions, n + 1, subcommandAndItsArguments)

    subcommandAndItsArguments ->
        (globalOptions, n, subcommandAndItsArguments)
  where
    (globalOptions, subcommandAndItsArguments') =
        List.span (\arg -> arg /= "--" && listToMaybe arg == pure '-') args

    n = fromIntegral (length globalOptions)

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

handleParseResult
    :: String
    -> Verbosity
    -> ColourOutput
    -> Options.ParserResult a
    -> IO a
handleParseResult command verbosity colour = \case
    Options.Success a ->
        pure a

    Options.Failure Options.ParserFailure{Options.execFailure} ->
        let (err, _, _) = execFailure command
        in dieFailedToParseOptions command verbosity colour stderr err

    Options.CompletionInvoked _ ->
        exitWith (ExitFailure 1) -- TODO: This is imposible case.
