{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:      CommandWrapper.Environment.Builder
-- Description: TODO: Module synopsis
-- Copyright:   (c) 2018 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- TODO: Module description.
module CommandWrapper.Environment.Builder
    (
    -- * Environment Builder
      EnvVars(..)
    , getEnv
    )
  where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Function (($), (.), const)
import Data.Functor ((<$>))
import Data.String (String)
import Data.Semigroup (Semigroup((<>)))
import Data.Monoid (Monoid(mempty))
import GHC.Generics (Generic)
import System.Environment (getEnvironment)

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap (fromList)

import CommandWrapper.Environment.Variable (EnvVarName, EnvVarValue)


newtype EnvVars = EnvVars
    { getEnvVars :: String -> HashMap EnvVarName EnvVarValue
    }
  deriving (Generic)

instance Semigroup EnvVars where
    EnvVars f <> EnvVars g = EnvVars $ \prefix ->
        f prefix <> g prefix

instance Monoid EnvVars where
    mempty = EnvVars (const mempty)

getEnv :: MonadIO io => io EnvVars
getEnv = EnvVars . const . HashMap.fromList <$>  liftIO getEnvironment
