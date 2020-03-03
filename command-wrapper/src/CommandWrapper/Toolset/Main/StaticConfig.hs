{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:      $Header$
-- Description: Configuration compiled into toolset executable.
-- Copyright:   (c) 2020 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Configuration compiled into toolset executable.
module CommandWrapper.Toolset.Main.StaticConfig
    ( StaticConfig(..)
    , def
    , doSystemConfigDirLookup
    , doSystemConfigDirLookupWithDefault
    , noSystemConfigDirLookup
    )
  where

import Control.Applicative (pure)
import Control.Monad ((>>=))
import Data.Bool (Bool(False))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Functor ((<$>), (<&>), fmap)
import GHC.Generics (Generic)
import System.IO (FilePath)

import qualified Data.Text as Text (unpack)

import CommandWrapper.Core.Environment
    ( CommandWrapperPrefix
    , CommandWrapperToolsetVarName(CommandWrapperSystemConfigDir)
    , ParseEnv
    , commandWrapperToolsetVarName
    , optionalVar
    )


data StaticConfig context = StaticConfig
    { lookupSystemConfigDir :: ParseEnv context (Maybe FilePath)
    , searchSystemPath :: Bool
    }
  deriving stock (Generic)

def :: StaticConfig context
def = StaticConfig
    { lookupSystemConfigDir = noSystemConfigDirLookup
    , searchSystemPath = False
    }

doSystemConfigDirLookup :: ParseEnv CommandWrapperPrefix (Maybe FilePath)
doSystemConfigDirLookup =
    commandWrapperToolsetVarName CommandWrapperSystemConfigDir
        >>= optionalVar'
  where
    optionalVar' varName' = fmap Text.unpack <$> optionalVar varName'

doSystemConfigDirLookupWithDefault
    :: FilePath
    -> ParseEnv CommandWrapperPrefix (Maybe FilePath)
doSystemConfigDirLookupWithDefault defaultConfigDir =
    doSystemConfigDirLookup <&> \case
        Nothing  -> Just defaultConfigDir
        Just dir -> Just dir

noSystemConfigDirLookup :: ParseEnv context (Maybe FilePath)
noSystemConfigDirLookup = pure Nothing
