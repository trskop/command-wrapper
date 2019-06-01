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

import Data.String (fromString)
import GHC.Generics (Generic)
import GHC.Exts (IsList(fromList))

import Data.Text (Text)
import Dhall ((>$<), (>*<))
import qualified Dhall
    ( Inject(injectWith)
    , InputType(InputType, declared, embed)
    , Interpret
    , InterpretOptions(InterpretOptions, fieldModifier)
    , inputRecord
    , inputFieldWith
    )
import qualified Dhall.Core as Dhall
    ( Expr
        ( App
        , List
        , ListLit
        , None
        , Optional
        , Some
        , Text
        , TextLit
        )
    )
import Data.Verbosity (Verbosity)
import Numeric.Natural (Natural)

import CommandWrapper.Config.Environment (EnvironmentVariable)
import CommandWrapper.Options.ColourOutput (ColourOutput)
import CommandWrapper.Options.Shell (Shell)

data NamedCommand = NamedCommand
    { name :: Text
    , descritpion :: Maybe Text
    , command :: Verbosity -> ColourOutput -> [Text] -> Command
    , completion :: Maybe (Shell -> Natural -> [Text] -> Command)
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
            >$< field "command" inputString
            >*< field "arguments" (inputList inputString)
            >*< field "environment" (Dhall.injectWith opts)
            >*< field "searchPath" (Dhall.injectWith opts)
            >*< field "workingDirectory" (inputMaybe inputString)
        )
      where
        adapt Command{..} =
            (command, (arguments, (environment, (searchPath, workingDirectory))))

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
            >$< field "command" inputString
            >*< field "arguments" (inputList inputString)
            >*< field "environment" (Dhall.injectWith opts)
        )
      where
        adapt SimpleCommand{..} = (command, (arguments, environment))

        field = Dhall.inputFieldWith . fieldModifier

-- {{{ Helper Functions -- Do Not Export --------------------------------------

inputString :: Dhall.InputType String
inputString = Dhall.InputType
    { declared = Dhall.Text
    , embed = Dhall.TextLit . fromString
    }

inputList :: Dhall.InputType a -> Dhall.InputType [a]
inputList Dhall.InputType{..} = Dhall.InputType
    { declared = Dhall.List `Dhall.App` declared
    , embed = Dhall.ListLit (Just declared) . fromList . fmap embed
    }

inputMaybe :: Dhall.InputType a -> Dhall.InputType (Maybe a)
inputMaybe Dhall.InputType{..} = Dhall.InputType
    { declared = Dhall.Optional `Dhall.App` declared
    , embed = maybe (Dhall.None `Dhall.App` declared) (Dhall.Some . embed)
    }

-- }}} Helper Functions -- Do Not Export --------------------------------------
