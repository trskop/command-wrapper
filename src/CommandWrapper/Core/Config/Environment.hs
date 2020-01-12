{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:      CommandWrapper.Core.Config.Environment
-- Description: Environment variables
-- Copyright:   (c) 2018-2020 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Environment variables.
module CommandWrapper.Core.Config.Environment
    ( EnvironmentVariable(..)
    , toTuple
    )
  where

import GHC.Generics (Generic)
import Text.Show (Show)

import Dhall (FromDhall, ToDhall)

import CommandWrapper.Core.Environment.Variable (EnvVarName, EnvVarValue)


-- | Represents environment variable @name=value@.
data EnvironmentVariable = EnvironmentVariable
    { name :: EnvVarName
    , value :: EnvVarValue
    }
  deriving stock (Generic, Show)
  deriving anyclass (FromDhall, ToDhall)

toTuple :: EnvironmentVariable -> (EnvVarName, EnvVarValue)
toTuple EnvironmentVariable{name, value} = (name, value)