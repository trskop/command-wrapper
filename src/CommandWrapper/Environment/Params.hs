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
import Data.Bifunctor (bimap, first)
import qualified Data.Char as Char (toLower)
import Data.Eq ((/=))
import Data.Function (($), (.))
import Data.Functor ((<$>), fmap)
import qualified Data.List as List (dropWhile)
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Semigroup (Semigroup((<>)))
import Data.String (String, fromString)
import Data.Tuple (snd, uncurry)
import Data.Version (Version, parseVersion, showVersion)
import GHC.Generics (Generic)
import System.IO (FilePath)
import Text.Show (Show, show)
import Text.ParserCombinators.ReadP (readP_to_S)

import Control.Monad.Except (throwError)
import qualified Data.CaseInsensitive as CaseInsensitive (mk)
import qualified Data.HashMap.Strict as HashMap (fromList, toList)
import qualified Data.Text as Text (unpack)
import Data.Verbosity (Verbosity)
import qualified Data.Verbosity as Verbosity (parse)

import CommandWrapper.Environment.Builder (EnvBuilder(EnvBuilder))
import CommandWrapper.Environment.Parser
    ( ParseEnv
    , ParseEnvError(ParseEnvError)
    , commandWrapperVar
    , commandWrapperVar'
    )
import CommandWrapper.Environment.Variable
    ( CommandWrapperVarName(..)
    , CommandWrapperPrefix
    , EnvVarName
    , EnvVarValue
    , commandWrapperPrefix
    , getCommandWrapperVarName
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

mkEnvVars :: Params -> EnvBuilder CommandWrapperPrefix
mkEnvVars Params{..} = EnvBuilder $ \prefix ->
    HashMap.fromList $ fmap (first $ getCommandWrapperVarName prefix)
        [ (CommandWrapperExe, fromString exePath)
        , (CommandWrapperName, fromString name)
        , (CommandWrapperSubcommand, fromString subcommand)
        , (CommandWrapperConfig, fromString config)
        , (CommandWrapperVerbosity, fromString $ Char.toLower <$> show verbosity)
        , (CommandWrapperColour, fromString $ Char.toLower <$> show colour)
        , (CommandWrapperVersion, fromString $ showVersion version)
        ]

commandWrapperEnv
    :: EnvBuilder CommandWrapperPrefix
    -> [(String, String)]
commandWrapperEnv (EnvBuilder mkEnv) =
    fromHashMap (mkEnv commandWrapperPrefix)
  where
    fromHashMap = fmap (bimap Text.unpack Text.unpack) . HashMap.toList

-- | Parse Command Wrapper environment\/protocol.
askParams :: ParseEnv CommandWrapperPrefix Params
askParams = Params
    <$> var CommandWrapperExe
    <*> var CommandWrapperName
    <*> var CommandWrapperSubcommand
    <*> var CommandWrapperConfig
    <*> verbosityVar CommandWrapperVerbosity
    <*> colourOutputVar CommandWrapperColour
    <*> versionVar CommandWrapperVersion
  where
    var = fmap Text.unpack . commandWrapperVar

    verbosityVar name = do
        commandWrapperVar' name >>= uncurry parseAsVerbosity

    colourOutputVar name =
        commandWrapperVar' name >>= uncurry parseAsColourOutput

    versionVar name =
        commandWrapperVar' name >>= uncurry parseAsVersion

    parseAsVerbosity
        :: EnvVarName
        -> EnvVarValue
        -> ParseEnv CommandWrapperPrefix Verbosity
    parseAsVerbosity name value =
        maybe (unableToParseVerbosity name value) pure
        . Verbosity.parse
        $ CaseInsensitive.mk value

    unableToParseVerbosity
        :: forall a
        .  EnvVarName
        -> EnvVarValue
        -> ParseEnv CommandWrapperPrefix a
    unableToParseVerbosity = throwParseEnvError "verbosity"

    parseAsColourOutput
        :: EnvVarName
        -> EnvVarValue
        -> ParseEnv CommandWrapperPrefix ColourOutput
    parseAsColourOutput name value =
        maybe (unableToParseColourOutput name value) pure
        . ColourOutput.parse
        $ CaseInsensitive.mk value

    unableToParseColourOutput
        :: forall a
        .  EnvVarName
        -> EnvVarValue
        -> ParseEnv CommandWrapperPrefix a
    unableToParseColourOutput = throwParseEnvError "colour output settings"

    parseAsVersion
        :: EnvVarName
        -> EnvVarValue
        -> ParseEnv CommandWrapperPrefix Version
    parseAsVersion name value = maybe (unableToParseVersion name value) pure
        $ case parseVersion' value of
            (v, "") : _ -> Just v
            _           -> Nothing
      where
        parseVersion' =
            List.dropWhile ((/= "") . snd)
            . readP_to_S parseVersion
            . Text.unpack

    unableToParseVersion
        :: forall a
        .  EnvVarName
        -> EnvVarValue
        -> ParseEnv CommandWrapperPrefix a
    unableToParseVersion = throwParseEnvError "version"

    throwParseEnvError
        :: forall a
        .  String
        -> EnvVarName
        -> EnvVarValue
        -> ParseEnv CommandWrapperPrefix a
    throwParseEnvError what name s = throwError (ParseEnvError name msg)
      where
        msg = "'" <> Text.unpack s <> "': Unable to parse " <> what <> "."
