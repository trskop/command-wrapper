{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
-- |
-- Module:      CommandWrapper.Options
-- Description: TODO: Module synopsis
-- Copyright:   (c) 2018 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- TODO: Module description.
module CommandWrapper.Options
    (
    -- * CommandWrapper Specific API
      Command
    , parseCommandWrapper

    -- * Generic API
    , parse

    -- ** Command Aliases
    , Alias(..)
    , applyAlias

    -- ** Helper Functions
    , splitArguments
    , execParserPure
    )
  where

import Control.Applicative (pure)
import Data.Bool ((&&))
import Data.Either (Either(Left, Right))
import Data.Eq ((/=), (==))
import Data.Functor (Functor, (<$>), fmap)
import Data.Function (($), (.))
import qualified Data.List as List (span)
import Data.Maybe (listToMaybe)
import Data.Monoid (Endo(Endo), mempty)
import Data.String (String)
import System.Environment (getArgs)
import System.IO (IO)

import qualified Mainplate (Command(..), ExternalCommand(..))
import qualified Options.Applicative as Options
    ( ParserInfo
    , ParserPrefs
    , ParserResult(Failure, Success)
    , handleParseResult
    , parserFailure
    )
import qualified Options.Applicative.Common as Options (runParserInfo)
import qualified Options.Applicative.Internal as Options (runP)

import qualified CommandWrapper.External as External (Command)
import qualified CommandWrapper.Internal as Internal (Command(..))
import CommandWrapper.Options.Alias (Alias(..), applyAlias)


type Command = Mainplate.Command External.Command Internal.Command

parseCommandWrapper
    :: Options.ParserPrefs
    -> Options.ParserInfo (Endo config)
    -> (Endo config -> IO [Alias])
    -> IO (Endo (Command config))
parseCommandWrapper parserPrefs parserInfo getAliases =
    parse parserPrefs parserInfo $ \updateConfig arguments -> do
        aliases <- getAliases updateConfig
        pure $ case arguments of
            [] ->
                mempty

            -- This is a very naive implementation and it will have to be
            -- redesigned at some point.
            cmd : args ->
                Endo $ case applyAlias aliases cmd args of
                    -- TODO: Refactor to use 'Internal.command'.
                    ("help", args') -> \case
                        Mainplate.Internal _ config ->
                            helpCommand args' config

                        Mainplate.External _ config ->
                            helpCommand args' config

                    ("config", args') -> \case
                        Mainplate.Internal _ config ->
                            configCommand args' config

                        Mainplate.External _ config ->
                            configCommand args' config

                    (cmd', args') -> \case
                        Mainplate.Internal _ config ->
                            externalCommand cmd' args' config

                        Mainplate.External _ config ->
                            externalCommand cmd' args' config
  where
    helpCommand = Mainplate.Internal . Internal.HelpCmommand
    configCommand = Mainplate.Internal . Internal.ConfigCommand

    externalCommand executable options =
        Mainplate.External Mainplate.ExternalCommand{..}

parse
    :: Functor mode
    => Options.ParserPrefs
    -> Options.ParserInfo (Endo config)
    -> (forall a. Endo config -> [String] -> IO (Endo (mode a)))
    -> IO (Endo (mode config))
parse parserPrefs parserInfo parseArguments = do
    (globalOptions, arguments) <- splitArguments <$> getArgs

    updateConfigEndo@(Endo updateConfig) <- Options.handleParseResult
        $ execParserPure parserPrefs parserInfo globalOptions

    Endo updateCommand <- parseArguments updateConfigEndo arguments

    pure $ Endo (fmap updateConfig . updateCommand)

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
