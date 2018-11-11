{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE LambdaCase #-}
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
import Data.Bifunctor (first)
import qualified Data.Char as Char (toLower)
import Data.Eq ((/=))
import Data.Function (($), (.))
import Data.Functor ((<$>), fmap)
import qualified Data.List as List (dropWhile)
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Semigroup (Semigroup((<>)))
import Data.String (String)
import Data.Tuple (snd, uncurry)
import Data.Version (Version, parseVersion, showVersion)
import GHC.Generics (Generic)
import System.IO (FilePath)
import Text.Show (Show, show)
import Text.ParserCombinators.ReadP (readP_to_S)

import Control.Monad.Except (throwError)
import qualified Data.CaseInsensitive as CaseInsensitive (mk)
import qualified Data.HashMap.Strict as HashMap (fromList, toList)
import Data.Verbosity (Verbosity)
import qualified Data.Verbosity as Verbosity (parse)

import CommandWrapper.Environment.Builder (EnvVars(EnvVars))
import CommandWrapper.Environment.Parser
    ( ParseEnv
    , ParseEnvError(ParseEnvError)
    , askCommandWrapperVar
    , askCommandWrapperVar'
    )
import CommandWrapper.Environment.Variable
    ( CommandWrapperVarName(..)
    , EnvVarName
    , EnvVarValue
    , commandWrapperPrefix
    , commandWrapperVarName
    )
import CommandWrapper.Options.ColourOutput (ColourOutput)
import qualified CommandWrapper.Options.ColourOutput as ColourOutput (parse)


data Params = Params
    { exePath :: FilePath
    , name :: String
    , subcommand :: String
    , config :: FilePath
    , verbosity :: Verbosity
    , colour :: ColourOutput
    , version :: Version
    }
  deriving (Generic, Show)

mkEnvVars :: Params -> EnvVars
mkEnvVars Params{..} = EnvVars $ \prefix ->
    HashMap.fromList $ fmap (first $ commandWrapperVarName prefix)
        [ (CommandWrapperExe, exePath)
        , (CommandWrapperName, name)
        , (CommandWrapperSubcommand, subcommand)
        , (CommandWrapperConfig, config)
        , (CommandWrapperVerbosity, Char.toLower <$> show verbosity)
        , (CommandWrapperColour, Char.toLower <$> show colour)
        , (CommandWrapperVersion, showVersion version)
        ]

commandWrapperEnv :: EnvVars -> [(EnvVarName, EnvVarValue)]
commandWrapperEnv (EnvVars mkEnv) = HashMap.toList (mkEnv commandWrapperPrefix)

-- | Parse Command Wrapper environment\/protocol.
askParams :: ParseEnv Params
askParams = Params
    <$> askCommandWrapperVar CommandWrapperExe
    <*> askCommandWrapperVar CommandWrapperName
    <*> askCommandWrapperVar CommandWrapperSubcommand
    <*> askCommandWrapperVar CommandWrapperConfig
    <*> askVerbosityVar CommandWrapperVerbosity
    <*> askColourOutputVar CommandWrapperColour
    <*> askVersionVar CommandWrapperVersion
  where
    askVerbosityVar name = do
        askCommandWrapperVar' name >>= uncurry parseAsVerbosity

    askColourOutputVar name =
        askCommandWrapperVar' name >>= uncurry parseAsColourOutput

    askVersionVar name =
        askCommandWrapperVar' name >>= uncurry parseAsVersion

    parseAsVerbosity :: EnvVarName -> EnvVarValue -> ParseEnv Verbosity
    parseAsVerbosity name value =
        maybe (unableToParseVerbosity name value) pure
        . Verbosity.parse
        $ CaseInsensitive.mk value

    unableToParseVerbosity :: forall a. EnvVarName -> EnvVarValue -> ParseEnv a
    unableToParseVerbosity = throwParseEnvError "verbosity"

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
    unableToParseColourOutput = throwParseEnvError "colour output settings"

    parseAsVersion :: EnvVarName -> EnvVarValue -> ParseEnv Version
    parseAsVersion name value = maybe (unableToParseVersion name value) pure
        $ case List.dropWhile ((/= "") . snd) (readP_to_S parseVersion value) of
            (v, "") : _ -> Just v
            _           -> Nothing

    unableToParseVersion :: forall a .  EnvVarName -> EnvVarValue -> ParseEnv a
    unableToParseVersion = throwParseEnvError "version"

    throwParseEnvError
        :: forall a
        .  String
        -> EnvVarName
        -> EnvVarValue
        -> ParseEnv a
    throwParseEnvError what name s = throwError (ParseEnvError name msg)
      where
        msg = "'" <> s <> "': Unable to parse " <> what <> "."
