{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Rank2Types #-}
--{-# LANGUAGE RecordWildCards #-}
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
import Data.Bool ((&&), otherwise)
import Data.Either (Either(Left, Right))
import Data.Eq ((/=), (==))
import Data.Functor (Functor, (<$>))
import Data.Function (($), (.))
import qualified Data.List as List (span)
import Data.Maybe (Maybe(Just), listToMaybe)
import Data.Monoid (Endo(Endo), (<>), mempty)
import Data.String (String)
import System.Environment (getArgs)
import System.IO (IO)

import Control.Comonad (Comonad, extract)
import Data.Monoid.Endo (E)
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
import qualified CommandWrapper.Internal as Internal (Command(..), command)
import CommandWrapper.Options.Alias (Alias(..), applyAlias)
import CommandWrapper.Options.GlobalMode (GlobalMode(..), runGlobalMode)


type Command = Mainplate.Command External.Command Internal.Command

parseCommandWrapper
    :: Options.ParserPrefs
    -> Options.ParserInfo (GlobalMode (Endo config))
    -> (Endo config -> IO [Alias])
    -> IO (Endo (Command config))
parseCommandWrapper parserPrefs parserInfo getAliases =
    parse parserPrefs parserInfo runGlobalMode' $ \updateConfig arguments -> do
        aliases <- getAliases updateConfig
        pure $ case arguments of
            [] ->
                mempty

            cmd : args ->
                Endo (parseCommand aliases cmd args)
  where
    parseCommand aliases cmd args =
        -- This is a very naive implementation and it will have to be
        -- redesigned at some point.
        case applyAlias aliases cmd args of
            (commandName, options)
              | Just command' <- Internal.command commandName options ->
                    switchTo Mainplate.Internal command'

              | otherwise ->
                    switchTo Mainplate.External Mainplate.ExternalCommand
                        { Mainplate.executable = commandName
                        , Mainplate.options
                        }

    switchTo :: (cmd -> config -> Command config) -> cmd -> E (Command config)
    switchTo constructor newCmd = constructor newCmd . \case
        Mainplate.Internal _oldCmd config -> config
        Mainplate.External _oldCmd config -> config

    runGlobalMode' :: GlobalMode (Endo config) -> Endo (Command config)
    runGlobalMode' = runGlobalMode $ \(Endo f) -> Endo $ \case
        Mainplate.Internal _oldCmd config ->
            Mainplate.Internal (Internal.HelpCmommand []) (f config)

        Mainplate.External _oldCmd config ->
            Mainplate.Internal (Internal.HelpCmommand []) (f config)

parse
    :: (Functor mode, Comonad globalMode)
    => Options.ParserPrefs
    -> Options.ParserInfo (globalMode (Endo config))
    -> (forall a. globalMode (Endo a) -> Endo (mode a))
    -> (forall a. Endo config -> [String] -> IO (Endo (mode a)))
    -> IO (Endo (mode config))
parse parserPrefs parserInfo fromGlobalMode parseArguments = do
    (globalOptions, arguments) <- splitArguments <$> getArgs

    globalMode <- Options.handleParseResult (execParserPure' globalOptions)

    let updateConfig = extract globalMode
    updateCommand <- parseArguments updateConfig arguments

    pure $ fromGlobalMode globalMode <> updateCommand
  where
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
