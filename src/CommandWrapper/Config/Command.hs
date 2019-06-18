-- |
-- Module:      CommandWrapper.Config.Command
-- Description: Command description suitable for execution
-- Copyright:   (c) 2018-2019 Peter Trško
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

import GHC.Generics (Generic)

import Data.Text (Text)
import Dhall ((>$<), (>*<))
import qualified Dhall
    ( Inject(injectWith)
    , InputType
    , Interpret
    , InterpretOptions(InterpretOptions, fieldModifier)
    , RecordInputType
    , inputRecord
    , inputFieldWith
    )
import Data.Verbosity (Verbosity)
import Numeric.Natural (Natural)

import CommandWrapper.Config.Environment (EnvironmentVariable)
import CommandWrapper.Config.NotifyWhen (NotifyWhen)
import CommandWrapper.Internal.Dhall as Dhall
import CommandWrapper.Options.ColourOutput (ColourOutput)
import CommandWrapper.Options.Shell (Shell)


data NamedCommand = NamedCommand
    { name :: Text
    , description :: Maybe Text
    , command :: Verbosity -> ColourOutput -> [Text] -> Command
    , completion :: Maybe (Shell -> Natural -> [Text] -> Command)
    , notifyWhen :: Maybe NotifyWhen
    }
  deriving stock (Generic)
  deriving anyclass (Dhall.Interpret)

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
  deriving anyclass (Dhall.Interpret)

instance Dhall.Inject Command where
    injectWith opts@Dhall.InterpretOptions{fieldModifier} = Dhall.inputRecord
        ( adapt
            >$< field "command" Dhall.inputString
            >*< field "arguments" (Dhall.inputList Dhall.inputString)
            >*< field "environment" (Dhall.injectWith opts)
            >*< field "searchPath" (Dhall.injectWith opts)
            >*< field "workingDirectory" (Dhall.inputMaybe Dhall.inputString)
        )
      where
        adapt Command{..} =
            (command, (arguments, (environment, (searchPath, workingDirectory))))

        field :: Text -> Dhall.InputType a -> Dhall.RecordInputType a
        field = Dhall.inputFieldWith . fieldModifier

data SimpleCommand = SimpleCommand
    { command :: FilePath
    , arguments :: [String]
    , environment :: [EnvironmentVariable]
    }
  deriving stock (Generic, Show)
  deriving anyclass (Dhall.Interpret)

instance Dhall.Inject SimpleCommand where
    injectWith opts@Dhall.InterpretOptions{fieldModifier} = Dhall.inputRecord
        ( adapt
            >$< field "command" Dhall.inputString
            >*< field "arguments" (Dhall.inputList Dhall.inputString)
            >*< field "environment" (Dhall.injectWith opts)
        )
      where
        adapt SimpleCommand{..} = (command, (arguments, environment))

        field :: Text -> Dhall.InputType a -> Dhall.RecordInputType a
        field = Dhall.inputFieldWith . fieldModifier
