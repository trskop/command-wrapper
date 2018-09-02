{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}
-- |
-- Module:      CommandWrapper.Config
-- Description: TODO: Module synopsis
-- Copyright:   (c) 2018 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- TODO: Module description.
module CommandWrapper.Config
    ( Config(..)
    , read
    , def
    )
  where

import Control.Applicative (pure)
import Control.Exception (Exception, catch)
import Data.Either (Either(Left, Right))
import Data.Function ((.), on)
import Data.Functor ((<$>))
import Data.Maybe (Maybe(Nothing))
import Data.Semigroup (Semigroup((<>)))
import Data.String (String)
import GHC.Generics (Generic)
import System.IO (FilePath, IO)
import Text.Show (Show, show)

import Data.Verbosity (Verbosity)
import qualified Data.Verbosity as Verbosity (Verbosity(..))
import Data.Verbosity.Class (HasVerbosity)
import qualified Data.Verbosity.Class (HasVerbosity(verbosity))
import Data.Generics.Product.Typed (typed)
import qualified Dhall (Interpret, InvalidType, auto, inputFile)
import qualified Dhall.Parser as Dhall (ParseError, Src)
import qualified Dhall.TypeCheck as Dhall (TypeError, X)

import qualified CommandWrapper.Options.Alias as Options


data Config = Config
    { aliases :: [Options.Alias]
    , searchPath :: [FilePath]
    , extraHelpMessage :: Maybe String
    , verbosity :: Verbosity
    }
  deriving (Generic, Show)

instance Dhall.Interpret Config

instance HasVerbosity Config where
    verbosity = typed

instance Semigroup Config where
    c1 <> c2 = Config
        { aliases = ((<>) `on` aliases) c1 c2
        , searchPath = ((<>) `on` searchPath) c2 c1
            -- Reverse order, more specific search path comes first.
        , extraHelpMessage = ((<>) `on` extraHelpMessage) c1 c2
        , verbosity = verbosity c1
        }

def :: Config
def = Config [] [] Nothing Verbosity.Normal

read :: FilePath -> IO (Either String Config)
read = catchDhallExceptions . Dhall.inputFile Dhall.auto
  where
    catchDhallExceptions parse =
        (Right <$> parse)
            `catch` handleException @Dhall.InvalidType
            `catch` handleException @Dhall.ParseError
            `catch` handleException @(Dhall.TypeError Dhall.Src Dhall.X)

    handleException :: Exception e => e -> IO (Either String Config)
    handleException = pure . Left . show
