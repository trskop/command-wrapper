{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:      CommandWrapper.Config.Global
-- Description: Global toolset configuration.
-- Copyright:   (c) 2018-2019 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Global toolset configuration, i.e. one used by @command-wrapper@ executable.
module CommandWrapper.Config.Global
    ( Config(..)
    , def
    , getAliases
    )
  where

import Data.Bool (Bool(False))
import Data.Maybe (Maybe(Nothing))
import Data.String (String)
import GHC.Generics (Generic)
import System.IO (FilePath)
import Text.Show (Show)

import Data.Verbosity (Verbosity)
import qualified Data.Verbosity as Verbosity (Verbosity(Normal))
import Data.Verbosity.Class (HasVerbosity)
import qualified Dhall (Interpret)

import CommandWrapper.Options.Alias (Alias)
import CommandWrapper.Options.ColourOutput (ColourOutput)


data Config = Config
    { aliases :: [Alias]
    , searchPath :: [FilePath]
    , description :: Maybe String
    , extraHelpMessage :: Maybe String
    , verbosity :: Verbosity
    , colourOutput :: ColourOutput
    , ignoreAliases :: Bool
    , changeDirectory :: Maybe FilePath
    }
  deriving stock (Generic, Show)
  deriving anyclass (Dhall.Interpret, HasVerbosity)

def :: ColourOutput -> Config
def colourOutput = Config
    { aliases = []
    , searchPath = []
    , description = Nothing
    , extraHelpMessage = Nothing
    , verbosity = Verbosity.Normal
    , colourOutput
    , ignoreAliases = False
    , changeDirectory = Nothing
    }

getAliases :: Config -> [Alias]
getAliases Config{aliases, ignoreAliases} =
    if ignoreAliases
        then []
        else aliases
