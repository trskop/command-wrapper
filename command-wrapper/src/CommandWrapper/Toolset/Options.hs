{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:      CommandWrapper.Toolset.Options
-- Description: Utilities for parsing command line options.
-- Copyright:   (c) 2018-2020 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Utilities for parsing command line options.
module CommandWrapper.Toolset.Options
    (
    -- * CommandWrapper Specific API
      Command
    , parseCommandWrapper

    -- ** Command Aliases
    , Alias(..)
    , applyAlias

    -- * Generic API
    , module CommandWrapper.Toolset.Options.Optparse
    , module CommandWrapper.Core.Options.Optparse
    , module CommandWrapper.Core.Options.GlobalMode
    )
  where

import Control.Applicative (pure)
import Data.Bool (otherwise)
import Data.Function ((.))
import Data.Maybe (Maybe(Just))
import Data.Monoid (Endo(Endo), mempty)
import System.IO (IO)

import Data.Monoid.Endo (E)
import qualified Mainplate (Command(..), ExternalCommand(..))
import qualified Options.Applicative as Options (ParserInfo, ParserPrefs)

import CommandWrapper.Core.Config.Alias (Alias(..), applyAlias)
import CommandWrapper.Core.Environment.AppNames (AppNames)
import CommandWrapper.Core.Options.GlobalMode
import CommandWrapper.Core.Options.Optparse
import qualified CommandWrapper.Toolset.ExternalSubcommand as External
    ( Command
    )
import qualified CommandWrapper.Toolset.InternalSubcommand as Internal
    ( Command(..)
    , command
    )
import CommandWrapper.Toolset.Options.Optparse


type Command = Mainplate.Command External.Command Internal.Command

parseCommandWrapper
    :: AppNames
    -> Options.ParserPrefs
    -> Options.ParserInfo (GlobalMode (Endo config))
    -> (Endo config -> IO [Alias])
    -> IO (Endo (Command config))
parseCommandWrapper appNames parserPrefs parserInfo getAliases =
    parse appNames parserPrefs parserInfo runGlobalMode'
        \updateConfig arguments -> do
            aliases <- getAliases updateConfig
            pure case arguments of
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
    runGlobalMode' = runGlobalMode helpMode versionMode
      where
        helpMode (Endo f) = Endo \case
            Mainplate.Internal _oldCmd config ->
                Mainplate.Internal (Internal.HelpCommand []) (f config)

            Mainplate.External _oldCmd config ->
                Mainplate.Internal (Internal.HelpCommand []) (f config)

        versionMode (Endo f) = Endo \case
            Mainplate.Internal _oldCmd config ->
                Mainplate.Internal (Internal.VersionCommand []) (f config)

            Mainplate.External _oldCmd config ->
                Mainplate.Internal (Internal.VersionCommand []) (f config)
