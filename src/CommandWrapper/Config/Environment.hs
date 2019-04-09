-- |
-- Module:      CommandWrapper.Config.Environment
-- Description: Environment variables
-- Copyright:   (c) 2018-2019 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Environment variables.
module CommandWrapper.Config.Environment
    ( EnvironmentVariable(..)
    , toTuple
    )
  where

import GHC.Generics (Generic)

import qualified Dhall (Inject, Interpret)

import CommandWrapper.Environment.Variable (EnvVarName, EnvVarValue)


data EnvironmentVariable = EnvironmentVariable
    { name :: EnvVarName
    , value :: EnvVarValue
    }
  deriving stock (Generic, Show)
  deriving anyclass (Dhall.Inject, Dhall.Interpret)

toTuple :: EnvironmentVariable -> (EnvVarName, EnvVarValue)
toTuple EnvironmentVariable{name, value} = (name, value)
