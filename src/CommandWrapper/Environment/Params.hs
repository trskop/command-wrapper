{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
-- |
-- Module:      CommandWrapper.Environment.Params
-- Description: TODO: Module synopsis
-- Copyright:   (c) 2018 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- TODO: Module description.
module CommandWrapper.Environment.Params
    (
    -- * Environment Builder
      Params(..)
    , mkEnvVars
    , commandWrapperEnv

    -- * Environment Parser
    , askParams
    )
  where

import Control.Applicative ((<*>), pure)
import Control.Monad ((>>=))
import qualified Data.Char as Char (toLower)
import Data.Function (($), (.))
import Data.Functor ((<$>))
import Data.Semigroup (Semigroup((<>)))
import Data.Maybe (maybe)
import GHC.Generics (Generic)
import System.IO (FilePath)
import Text.Show (Show, show)

import Control.Monad.Except (throwError)
import qualified Data.CaseInsensitive as CaseInsensitive (mk)
import qualified Data.HashMap.Strict as HashMap (fromList, toList)
import Data.Verbosity (Verbosity)
import qualified Data.Verbosity as Verbosity (parse)

import CommandWrapper.Environment.Builder (EnvVars(EnvVars))
import CommandWrapper.Environment.Parser
    ( ParseEnv
    , ParseEnvError(ParseEnvError)
    , askVar
    )
import CommandWrapper.Environment.Variable (EnvVarName, EnvVarValue)
import CommandWrapper.Options.ColourOutput (ColourOutput)
import qualified CommandWrapper.Options.ColourOutput as ColourOutput (parse)


data Params = Params
    { exePath :: FilePath
    , name :: FilePath
    , config :: FilePath
    , verbosity :: Verbosity
    , colour :: ColourOutput
    }
  deriving (Generic, Show)

mkEnvVars :: Params -> EnvVars
mkEnvVars Params{..} = EnvVars $ \prefix ->
    HashMap.fromList
      [ (prefix <> "_EXE", exePath)
      , (prefix <> "_NAME", name)
      , (prefix <> "_CONFIG", config)
      , (prefix <> "_VERBOSITY", Char.toLower <$> show verbosity)
      , (prefix <> "_COLOUR", Char.toLower <$> show colour)
      ]

commandWrapperEnv :: EnvVars -> [(EnvVarName, EnvVarValue)]
commandWrapperEnv (EnvVars mkEnv) = HashMap.toList (mkEnv "COMMAND_WRAPPER")

askParams :: ParseEnv Params
askParams = Params
    <$> askVar "COMMAND_WRAPPER_EXE"
    <*> askVar "COMMAND_WRAPPER_NAME"
    <*> askVar "COMMAND_WRAPPER_CONFIG"
    <*> askVerbosityVar "COMMAND_WRAPPER_VERBOSITY"
    <*> askColourOutputVar "COMMAND_WRAPPER_COLOUR"
  where
    askVerbosityVar name = askVar name >>= parseAsVerbosity name

    askColourOutputVar name = askVar name >>= parseAsColourOutput name

    parseAsVerbosity :: EnvVarName -> EnvVarValue -> ParseEnv Verbosity
    parseAsVerbosity name value =
        maybe (unableToParseVerbosity name value) pure
        . Verbosity.parse
        $ CaseInsensitive.mk value

    unableToParseVerbosity :: forall a. EnvVarName -> EnvVarValue -> ParseEnv a
    unableToParseVerbosity name s = throwError
        $ ParseEnvError name ("'" <> s <> "': Unable to parse as verbosity.")

    parseAsColourOutput :: EnvVarName -> EnvVarValue -> ParseEnv ColourOutput
    parseAsColourOutput name value =
        maybe (unableToParseColourOutput name value) pure
        . ColourOutput.parse
        $ CaseInsensitive.mk value

    unableToParseColourOutput
        :: forall a
        .  EnvVarName
        -> EnvVarValue
        -> ParseEnv a
    unableToParseColourOutput name s = throwError $ ParseEnvError name
        ("'" <> s <> "': Unable to parse colour output settings.")
