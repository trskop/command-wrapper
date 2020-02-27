{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:      CommandWrapper.Toolset.Options.Optparse
-- Description: Utilities for parsing command line options.
-- Copyright:   (c) 2018-2020 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Utilities for parsing command line options.
module CommandWrapper.Toolset.Options.Optparse
    ( internalSubcommandParse
    , bashCompleter
    )
  where

import Control.Applicative (pure)
import Control.Monad ((>=>))
import Data.Foldable (length)
import Data.Function (($), (.))
import Data.Functor ((<$>))
import qualified Data.List as List (drop)
import Data.Monoid (Endo, (<>))
import Data.String (String)
import System.IO (IO)

import qualified Options.Applicative as Options (ParserInfo, ParserPrefs)
import qualified Options.Applicative.Builder.Completer as Options
    ( bashCompleter
    )
import qualified Options.Applicative.Types as Options (Completer(runCompleter))
import System.Directory (doesDirectoryExist)

import CommandWrapper.Config.Global (Config(Config, colourOutput, verbosity))
import CommandWrapper.Core.Environment.AppNames (AppNames(AppNames, usedName))
import CommandWrapper.Core.Options.Optparse (execParserPure, handleParseResult)


internalSubcommandParse
    :: AppNames
    -> Config
    -> String
    -> Options.ParserPrefs
    -> Options.ParserInfo (Endo (mode config))
    -> [String]
    -> IO (Endo (mode config))
internalSubcommandParse appNames config subcommand parserPrefs parserInfo =
    handleParseResult' appNames config . execParserPure parserPrefs parserInfo
  where
    handleParseResult' AppNames{usedName} Config{colourOutput, verbosity} =
        handleParseResult (usedName <> " " <> subcommand) verbosity
            colourOutput

bashCompleter :: String -> String -> String -> IO [String]
bashCompleter action prefix =
    ( Options.runCompleter (Options.bashCompleter action)
        . List.drop (length prefix)
    )
    >=> \case
        -- If there is only one completion option, and it is a directory, by
        -- appending '/' we'll force completion to descend into that directory.
        --
        -- TODO: This behaviour should be configurable!
        [path] -> do
            isDirectory <- doesDirectoryExist path
            pure if isDirectory
                then [prefix <> path <> "/"]
                else [prefix <> path]

        r ->
            pure $ (prefix <>) <$> r
