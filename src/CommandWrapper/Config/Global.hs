{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:      CommandWrapper.Config.Global
-- Description: Global configuration file.
-- Copyright:   (c) 2018-2019 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Global configuration file, i.e. one used by @command-wrapper@ executable.
module CommandWrapper.Config.Global
    ( Config(..)
    , read
    , def
    )
  where

import Control.Applicative (pure)
import Control.Exception (Exception, catch)
import Data.Either (Either(Left, Right))
import Data.Function (($), (.), on)
import Data.Functor ((<$>))
import Data.Maybe (Maybe(Nothing))
import Data.Monoid (Last(Last, getLast))
import Data.Semigroup (Semigroup((<>)))
import Data.String (String)
import GHC.Generics (Generic)
import System.IO (FilePath, IO)
import Text.Show (Show, show)

import Data.Verbosity (Verbosity)
import qualified Data.Verbosity as Verbosity (Verbosity(Normal))
import Data.Verbosity.Class (HasVerbosity)
import qualified Dhall (Interpret, InvalidType, auto, inputFile)
import qualified Dhall.Parser as Dhall (ParseError, Src)
import qualified Dhall.TypeCheck as Dhall (TypeError, X)

import CommandWrapper.Options.Alias (Alias)
import CommandWrapper.Options.ColourOutput (ColourOutput)


data Config = Config
    { aliases :: [Alias]
    , searchPath :: [FilePath]
    , description :: Maybe String
    , extraHelpMessage :: Maybe String
    , verbosity :: Verbosity
    , colourOutput :: Maybe ColourOutput
    }
  deriving stock (Generic, Show)
  deriving anyclass (Dhall.Interpret, HasVerbosity)

instance Semigroup Config where
    c1 <> c2@Config{verbosity} = Config
        { aliases = ((<>) `on` aliases) c1 c2
        , searchPath = ((<>) `on` searchPath) c2 c1
            -- Reverse order, more specific search path comes first.
        , description = getLast $ ((<>) `on` (Last . description)) c1 c2
        , extraHelpMessage = ((<>) `on` extraHelpMessage) c1 c2
        , verbosity
        , colourOutput = getLast $ ((<>) `on` (Last . colourOutput)) c1 c2
        }

def :: Config
def = Config
    { aliases = []
    , searchPath = []
    , description = Nothing
    , extraHelpMessage = Nothing
    , verbosity = Verbosity.Normal
    , colourOutput = Nothing
    }

read :: FilePath -> IO (Either String Config)
read = catchDhallExceptions . Dhall.inputFile Dhall.auto
  where
    catchDhallExceptions parse =
        (Right <$> parse)
            `catch` handleException @(Dhall.InvalidType Dhall.Src Dhall.X)
            `catch` handleException @Dhall.ParseError
            `catch` handleException @(Dhall.TypeError Dhall.Src Dhall.X)

    handleException :: Exception e => e -> IO (Either String Config)
    handleException = pure . Left . show
