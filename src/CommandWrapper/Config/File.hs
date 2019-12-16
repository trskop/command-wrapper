{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:      CommandWrapper.Config.File
-- Description: Global toolset configuration file.
-- Copyright:   (c) 2018-2019 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Global toolset configuration file, i.e. one used by @command-wrapper@
-- executable.
module CommandWrapper.Config.File
    ( Config(..)
    , apply
    , read
    )
  where

import Control.Applicative ((<*>), pure)
import Control.Exception (Exception, catch)
import Data.Either (Either(Left, Right))
import Data.Function (($), (.), on)
import Data.Functor ((<$>))
import Data.Maybe (Maybe, fromMaybe)
import Data.Monoid (Last(Last, getLast))
import Data.Semigroup (Semigroup((<>)))
import Data.String (String)
import Data.Void (Void)
import GHC.Generics (Generic)
import System.IO (FilePath, IO)
import Text.Show (Show, show)

import Data.Monoid.Endo (E)
import Data.Verbosity (Verbosity)
import Dhall (FromDhall)
import qualified Dhall (InvalidDecoder, auto, inputFile)
import qualified Dhall.Parser as Dhall (ParseError, Src)
import qualified Dhall.TypeCheck as Dhall (TypeError)

import CommandWrapper.Options.Alias (Alias)
import CommandWrapper.Options.ColourOutput (ColourOutput)
import qualified CommandWrapper.Config.Global as Global (Config(..))


-- | Represents toolset configuration file.
data Config = Config
    { aliases :: [Alias]
    , searchPath :: [FilePath]
    , manPath :: [FilePath]
    , description :: Maybe String
    , extraHelpMessage :: Maybe String
    , verbosity :: Maybe Verbosity
    , colourOutput :: Maybe ColourOutput
    }
  deriving stock (Generic, Show)
  deriving anyclass (FromDhall)

-- | Apply configuration file to internal configuration that represents not
-- only configuration file, but also command line options, see
-- "CommandWrapper.Config.Global" module for more information.
apply :: Config -> E Global.Config
apply Config{..} config = config
    { Global.aliases = gAliases <> aliases
    , Global.searchPath = gSearchPath <> searchPath
    , Global.manPath = gManPath <> manPath
    , Global.description = getLast $ ((<>) `on` Last) gDescription description
    , Global.extraHelpMessage = (<>) <$> gExtraHelpMessage <*> extraHelpMessage
    , Global.verbosity = fromMaybe gVerbosity verbosity
    , Global.colourOutput = fromMaybe gColourOutput colourOutput
    }
  where
    Global.Config
        { Global.aliases = gAliases
        , Global.searchPath = gSearchPath
        , Global.manPath = gManPath
        , Global.description = gDescription
        , Global.extraHelpMessage = gExtraHelpMessage
        , Global.verbosity = gVerbosity
        , Global.colourOutput = gColourOutput
        }
        = config

-- | Read and parse Dhall configuration file.
read :: FilePath -> IO (Either String Config)
read = catchDhallExceptions . Dhall.inputFile Dhall.auto
  where
    catchDhallExceptions parse =
        (Right <$> parse)
            `catch` handleException @(Dhall.InvalidDecoder Dhall.Src Void)
            `catch` handleException @Dhall.ParseError
            `catch` handleException @(Dhall.TypeError Dhall.Src Void)

    handleException :: Exception e => e -> IO (Either String Config)
    handleException = pure . Left . show
