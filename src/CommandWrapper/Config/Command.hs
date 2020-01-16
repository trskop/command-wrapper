{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:      CommandWrapper.Config.Command
-- Description: Command description suitable for execution
-- Copyright:   (c) 2018-2020 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Command description suitable for execution.
module CommandWrapper.Config.Command
    ( Command(..)
    , SimpleCommand(..)
    , NamedCommand(..)
    , isNamed
    )
  where

import Data.Bool (Bool)
import Data.Eq ((==))
import Data.Function ((.))
import Data.Maybe (Maybe)
import Data.String (String)
import GHC.Generics (Generic)
import System.IO (FilePath)
import Text.Show (Show)

import Data.Text (Text)
import Dhall ((>$<), (>*<))
import Dhall (FromDhall, ToDhall)
import qualified Dhall
    ( Encoder
    , InterpretOptions(InterpretOptions, fieldModifier)
    , RecordEncoder
    , ToDhall(injectWith)
    , encodeFieldWith
    , recordEncoder
    )
import Data.Verbosity (Verbosity)
import Numeric.Natural (Natural)

import CommandWrapper.Core.Config.ColourOutput (ColourOutput)
import CommandWrapper.Core.Config.Environment (EnvironmentVariable)
import CommandWrapper.Core.Config.Shell (Shell)
import CommandWrapper.Config.NotifyWhen (NotifyWhen)
import CommandWrapper.Internal.Dhall as Dhall


data NamedCommand = NamedCommand
    { name :: Text
    , description :: Maybe Text
    , command :: Verbosity -> ColourOutput -> [Text] -> Command
    , completion :: Maybe (Shell -> Natural -> [Text] -> Command)
    , notifyWhen :: Maybe NotifyWhen
    }
  deriving stock (Generic)
  deriving anyclass (FromDhall)

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
  deriving stock (Generic, Show)
  deriving anyclass (FromDhall)

instance ToDhall Command where
    injectWith opts@Dhall.InterpretOptions{fieldModifier} = Dhall.recordEncoder
        ( adapt
            >$< field "command" Dhall.inputString
            >*< field "arguments" (Dhall.inputList Dhall.inputString)
            >*< field "environment" (Dhall.injectWith opts)
            >*< field "searchPath" (Dhall.injectWith opts)
            >*< field "workingDirectory"
                (Dhall.inputMaybe Dhall.inputString)
        )
      where
        adapt Command{..} =
            (command, (arguments, (environment, (searchPath, workingDirectory))))

        field :: Text -> Dhall.Encoder a -> Dhall.RecordEncoder a
        field = Dhall.encodeFieldWith . fieldModifier

data SimpleCommand = SimpleCommand
    { command :: FilePath
    , arguments :: [String]
    , environment :: [EnvironmentVariable]
    }
  deriving stock (Generic, Show)
  deriving anyclass (FromDhall)

instance ToDhall SimpleCommand where
    injectWith opts@Dhall.InterpretOptions{fieldModifier} = Dhall.recordEncoder
        ( adapt
            >$< field "command" Dhall.inputString
            >*< field "arguments" (Dhall.inputList Dhall.inputString)
            >*< field "environment" (Dhall.injectWith opts)
        )
      where
        adapt SimpleCommand{..} = (command, (arguments, environment))

        field :: Text -> Dhall.Encoder a -> Dhall.RecordEncoder a
        field = Dhall.encodeFieldWith . fieldModifier
