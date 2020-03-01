-- |
-- Module:      $Header$
-- Description: Run Command Wrapper subcommands written as Shake rules.
-- Copyright:   (c) 2020 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions; POSIX.
--
-- Run Command Wrapper subcommands written as Shake rules.
module CommandWrapper.Subcommand.Shake.Options
    ( runShakeSubcommand
    , commandWrapperVerbosityToShake
    , commandWrapperColourOutputToShake
    )
  where

import Control.Applicative (pure)
import Data.Bool (Bool)
import System.IO (IO, stdout)

import Data.Generics.Product.Typed (HasType, getTyped)
import Development.Shake (ShakeOptions, shake, shakeOptions)
import qualified Development.Shake as Shake
    ( Rules
    , ShakeOptions(shakeColor, shakeVerbosity)
    , Verbosity(Diagnostic, Silent, Verbose, Warn)
    )

import CommandWrapper.Subcommand.Prelude (SubcommandProps, runSubcommand)
import qualified CommandWrapper.Subcommand.Prelude as Subcommand


-- | Parse subcommand options and run Shake rules.
runShakeSubcommand
    :: forall params action
    .  HasType Subcommand.Params params
    => (ShakeOptions -> params -> IO ShakeOptions)
    -> SubcommandProps params action
    -> (params -> action -> Shake.Rules ())
    -> IO ()
runShakeSubcommand toShakeOptions props rules =
    runSubcommand props \params action -> do
        defaultShakeOptions <- do
            let params' = getTyped @Subcommand.Params params
            shakeColor <- commandWrapperColourOutputToShake
                (Subcommand.colour params')
            pure shakeOptions
                { Shake.shakeColor
                , Shake.shakeVerbosity =
                    commandWrapperVerbosityToShake
                        (Subcommand.verbosity params')
                }

        shakeOptions' <- toShakeOptions defaultShakeOptions params
        shake shakeOptions' (rules params action)

-- | Be aware that the mapping is not 1:1. Shake has a more verbosity options
-- than Command Wrapper does.
commandWrapperVerbosityToShake :: Subcommand.Verbosity -> Shake.Verbosity
commandWrapperVerbosityToShake = \case
    Subcommand.Silent -> Shake.Silent
    Subcommand.Normal -> Shake.Warn
    Subcommand.Verbose -> Shake.Verbose
    Subcommand.Annoying -> Shake.Diagnostic

-- | Shake doesn't have notion equivalent to Subcommand.Auto', hence the
-- 'Maybe'
commandWrapperColourOutputToShake :: Subcommand.ColourOutput -> IO Bool
commandWrapperColourOutputToShake = Subcommand.shouldUseColours stdout
