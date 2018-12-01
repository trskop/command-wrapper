{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
-- |
-- Module:      CommandWrapper.Config.Command
-- Description: Command description suitable for execution
-- Copyright:   (c) 2018 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Command description suitable for execution.
module CommandWrapper.Config.Command
    ( Command(..)
    , NamedCommand(..)
    , isNamed
    )
  where

import GHC.Generics (Generic)

import Data.Text (Text)
import qualified Dhall (Interpret)
import Data.Verbosity (Verbosity)

import CommandWrapper.Config.Environment (EnvironmentVariable)
import CommandWrapper.Options.ColourOutput (ColourOutput)


data NamedCommand = NamedCommand
    { name :: Text
    , command :: Verbosity -> ColourOutput -> [Text] -> Command
    }
  deriving (Generic)

instance Dhall.Interpret NamedCommand

isNamed :: Text -> NamedCommand -> Bool
isNamed expectedName NamedCommand{name} = name == expectedName

data Command = Command
    { command :: FilePath
    , arguments :: [String]
    , environment :: [EnvironmentVariable]
    -- ^ List of variables to be added to current environment before executing
    -- command.
    , searchPath :: Bool
    -- ^ Search @PATH@ when looking for 'command'?
    , workingDirectory :: Maybe FilePath
    }
  deriving (Generic, Show)

instance Dhall.Interpret Command
