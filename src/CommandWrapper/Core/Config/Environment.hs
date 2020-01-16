{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:      CommandWrapper.Core.Config.Environment
-- Description: Data type representing environment variable.
-- Copyright:   (c) 2018-2020 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Data type representing environment variable.
module CommandWrapper.Core.Config.Environment
    ( EnvironmentVariable(..)
    , toTuple
    , fromTuple
    )
  where

import GHC.Generics (Generic)
import Text.Show (Show)

import Dhall (FromDhall, ToDhall)

import CommandWrapper.Core.Environment.Variable (EnvVarName, EnvVarValue)


-- | Represents environment variable @name=value@.
--
-- For example @FOO=bar@ would be:
--
-- @
-- fooVar :: 'EnvironmentVariable'
-- fooVar = 'EnvironmentVariable'
--    { 'name' = \"FOO\"
--    , 'value' = \"bar\"
--    }
-- @
--
-- Encoded into Dhall (with type annotation) it would be:
--
-- > { name = "FOO", value = "bar" } : { name : Text, value : Text }
data EnvironmentVariable = EnvironmentVariable
    { name :: EnvVarName
    , value :: EnvVarValue
    }
  deriving stock (Generic, Show)
  deriving anyclass (FromDhall, ToDhall)

-- | Standard functions usually use tuples to represent environment variables.
-- This function converts 'EnvironmentVariable' into such representation. Dual
-- function is 'fromTuple'
toTuple :: EnvironmentVariable -> (EnvVarName, EnvVarValue)
toTuple EnvironmentVariable{name, value} = (name, value)

-- | Standard functions usually use tuples to represent environment variables.
-- This function converts such representation into 'EnvironmentVariable'. Dual
-- function is 'toTuple'
fromTuple :: (EnvVarName, EnvVarValue) -> EnvironmentVariable
fromTuple (name, value) = EnvironmentVariable{..}
