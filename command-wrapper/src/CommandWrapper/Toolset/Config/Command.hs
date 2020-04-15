-- |
-- Module:      $Header$
-- Description: Command description suitable for execution
-- Copyright:   (c) 2018-2020 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions; POSIX.
--
-- Command description suitable for execution.
module CommandWrapper.Toolset.Config.Command
    (
    -- * Command
      Command(..)
    , injectCommand
    , fromSimpleCommand

    -- ** Execute Command
    , ExecuteCommandError(..)
    , executeCommand

    -- * Simple Command
    , SimpleCommand(..)
    , injectSimpleCommand

    -- * Named Command
    , NamedCommand(..)
    , isNamed
    )
  where

import Control.Applicative (pure)
import Control.Monad ((>>=), join)
import Data.Bifunctor (bimap)
import Data.Bool (Bool(True))
import Data.Eq ((==))
import Data.Function ((.))
import Data.Functor ((<$), (<$>), (<&>))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Semigroup ((<>))
import Data.String (String)
import Data.Traversable (for)
import GHC.Generics (Generic)
import System.Environment (getEnvironment)
import System.IO (IO, FilePath)
import System.IO.Error (IOError, catchIOError)
import Text.Show (Show)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map (fromList, toList)
import Data.Text (Text)
import qualified Data.Text as Text (unpack)
import Dhall ((>$<), (>*<))
import Dhall (FromDhall, ToDhall)
import qualified Dhall
    ( Encoder
    , InputNormalizer
    , InterpretOptions(InterpretOptions, fieldModifier)
    , RecordEncoder
    , ToDhall(injectWith)
    , defaultInterpretOptions
    , encodeFieldWith
    , recordEncoder
    )
import Numeric.Natural (Natural)
import System.Directory (setCurrentDirectory)
import qualified System.Posix as Posix (executeFile)

import CommandWrapper.Core.Config.ColourOutput (ColourOutput)
import CommandWrapper.Core.Config.Environment (EnvironmentVariable)
import qualified CommandWrapper.Core.Config.Environment as EnvironmentVariable
    ( toTuple
    )
import CommandWrapper.Core.Config.Shell (Shell)
import CommandWrapper.Core.Config.Verbosity (Verbosity)
import CommandWrapper.Core.Dhall as Dhall
import CommandWrapper.Toolset.Config.NotifyWhen (NotifyWhen)


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
    injectWith :: Dhall.InputNormalizer -> Dhall.Encoder Command
    injectWith = injectCommand Dhall.defaultInterpretOptions
    {-# INLINE injectWith #-}

injectCommand
    :: Dhall.InterpretOptions
    -> Dhall.InputNormalizer
    -> Dhall.Encoder Command
injectCommand Dhall.InterpretOptions{fieldModifier} = \inputNormalizer ->
    Dhall.recordEncoder
        ( adapt
            >$< field "command" Dhall.inputString
            >*< field "arguments" (Dhall.inputList Dhall.inputString)
            >*< field "environment" (Dhall.injectWith inputNormalizer)
            >*< field "searchPath" (Dhall.injectWith inputNormalizer)
            >*< field "workingDirectory" (Dhall.inputMaybe Dhall.inputString)
        )
  where
    adapt Command{..} =
        (command, (arguments, (environment, (searchPath, workingDirectory))))

    field :: Text -> Dhall.Encoder a -> Dhall.RecordEncoder a
    field = Dhall.encodeFieldWith . fieldModifier
{-# INLINEABLE injectCommand #-}

data SimpleCommand = SimpleCommand
    { command :: FilePath
    , arguments :: [String]
    , environment :: [EnvironmentVariable]
    }
  deriving stock (Generic, Show)
  deriving anyclass (FromDhall)

instance ToDhall SimpleCommand where
    injectWith :: Dhall.InputNormalizer -> Dhall.Encoder SimpleCommand
    injectWith = injectSimpleCommand Dhall.defaultInterpretOptions
    {-# INLINE injectWith #-}

injectSimpleCommand
    :: Dhall.InterpretOptions
    -> Dhall.InputNormalizer
    -> Dhall.Encoder SimpleCommand
injectSimpleCommand Dhall.InterpretOptions{fieldModifier} = \inputNormalizer ->
    Dhall.recordEncoder
        ( adapt
            >$< field "command" Dhall.inputString
            >*< field "arguments" (Dhall.inputList Dhall.inputString)
            >*< field "environment" (Dhall.injectWith inputNormalizer)
        )
  where
    adapt SimpleCommand{..} = (command, (arguments, environment))

    field :: Text -> Dhall.Encoder a -> Dhall.RecordEncoder a
    field = Dhall.encodeFieldWith . fieldModifier
{-# INLINEABLE injectSimpleCommand #-}

-- | Smart constructor for 'Command' that uses following defaults for fields
-- missing in 'SimpleCommand':
--
-- @
-- 'searchPath' = True  -- Search PATH environment variable for 'command'.
-- 'workingDirectory' = Nothing  -- Use current directory
-- @
fromSimpleCommand :: SimpleCommand -> Command
fromSimpleCommand SimpleCommand{..} = Command
    { command
    , arguments
    , environment
    , searchPath = True
    , workingDirectory = Nothing
    }

data ExecuteCommandError
    = FailedToSetCurrentDirectory FilePath IOError
    | FailedToExecuteCommand FilePath Bool [String] [(String, String)] IOError

-- | Executing 'Command' consists of following steps:
--
-- * Change working directory if the value of 'workingDirectory' is 'Just'.
--   If this fails then the function returns 'FailedToSetCurrentDirectory'
-- * Read current environment, apply transformation function, and then add
--   variables specified in 'environment' field.
-- * Execute 'command' with 'arguments' passed to it and environment set to the
--   value generated in previous step.  This step respects 'searchPath' when
--   trying to find 'command'.
executeCommand
    :: (Map String String -> Map String String)
    -- ^ Pre-process environment variables before inserting definitions from
    -- 'environment'.  This is useful to remove Command Wrapper specific
    -- environment variables that we don't want to be propagated to the
    -- executed command.
    -> Command
    -- ^ Command to execute.
    -> IO ExecuteCommandError
    -- ^ Function doesn't return if successful, i.e. 'ExecuteCommandError' is
    -- returned only if it fails.
executeCommand transformEnvironment Command{..} =
    changeWorkingDirectory >>= \case
        Nothing ->
            mkEnvironment >>= executeFile
        Just err ->
            pure err
  where
    changeWorkingDirectory :: IO (Maybe ExecuteCommandError)
    changeWorkingDirectory = join <$> for workingDirectory \dir ->
        (Nothing <$ setCurrentDirectory dir)
           `catchIOError` (pure . Just . FailedToSetCurrentDirectory dir)

    mkEnvironment :: IO [(String, String)]
    mkEnvironment = getEnvironment <&> \env ->
        Map.toList (transformEnvironment (Map.fromList env) <> additionalVars)
      where
        additionalVars = Map.fromList (varToTuple <$> environment)

        varToTuple =
            bimap Text.unpack Text.unpack . EnvironmentVariable.toTuple

    executeFile :: [(String, String)] -> IO ExecuteCommandError
    executeFile env =
        Posix.executeFile command searchPath arguments (Just env)
            `catchIOError`
                (pure . FailedToExecuteCommand command searchPath arguments env)
