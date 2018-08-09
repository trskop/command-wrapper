{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
-- |
-- Module:      CommandWrapper.Environment
-- Description: TODO: Module synopsis
-- Copyright:   (c) 2018 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- TODO: Module description.
module CommandWrapper.Environment
    ( EnvVarName
    , EnvVarValue

    -- * Environment Builder
    , EnvVars(..)
    , Params(..)
    , envVars

    -- * Environment Parser
    , ParseEnv(..)
    , ParseEnvError(..)
    , parseEnv
    , askVar
    , askOptionalVar

    -- * Application Names
    , AppNames(..)
    , appNames
    )
  where

import Prelude

import Control.Applicative -- (Alternative, Applicative, pure)
import Control.Monad (Monad, MonadPlus, (>>=))
import Control.Monad.Fail (MonadFail(fail))
import Data.Either (Either)
import Data.Function (($), (.), const)
import Data.Functor (Functor, (<&>))
import Data.Functor.Identity (Identity(Identity))
import Data.String (String)
import Data.Semigroup (Semigroup((<>)))
import Data.Maybe (Maybe, maybe)
import Data.Monoid (Monoid(mempty))
import System.Environment (getProgName)
import System.IO (FilePath)
import Text.Show (Show)

import Control.Monad.Reader (ReaderT(ReaderT))
import Control.Monad.Except (Except, ExceptT(ExceptT), MonadError, throwError)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap (fromList, lookup)
import System.Environment.Executable (ScriptPath(..), getScriptPath)
import System.FilePath (takeFileName)


type EnvVarName = String
type EnvVarValue = String

-- {{{ Environment Builder ----------------------------------------------------

newtype EnvVars = EnvVars
    { getEnvVars :: String -> HashMap EnvVarName EnvVarValue
    }

instance Semigroup EnvVars where
    EnvVars f <> EnvVars g = EnvVars $ \prefix ->
        f prefix <> g prefix

instance Monoid EnvVars where
    mempty = EnvVars $ const mempty

data Params = Params
    { projectRoot :: FilePath
    , exePath :: FilePath
    , name :: FilePath
    , config :: FilePath
    }

envVars :: Params -> EnvVars
envVars Params{..} = EnvVars $ \prefix ->
    HashMap.fromList
      [ (prefix <> "_EXE", exePath)
      , (prefix <> "_PROJECT_ROOT", projectRoot)
      , (prefix <> "_NAME", name)
      , (prefix <> "_CONFIG", config)
      ]

-- }}} Environment Builder ----------------------------------------------------

-- {{{ Environment Parser -----------------------------------------------------

data ParseEnvError
    = ParseEnvError EnvVarName String
    | MissingEnvVarError EnvVarName
    | ErrorMessage String
    | UnknownError
  deriving (Show)

instance Semigroup ParseEnvError where
    e <> UnknownError = e
    _ <> e            = e

instance Monoid ParseEnvError where
    mempty = UnknownError

newtype ParseEnv a = ParseEnv
    { parseEnv'
        :: ReaderT (HashMap EnvVarName EnvVarValue) (Except ParseEnvError) a
    }
  deriving
    ( Alternative
    , Applicative
    , Functor
    , Monad
    , MonadError ParseEnvError
    , MonadPlus
    )

instance MonadFail ParseEnv where
    fail = throwError . ErrorMessage

askVar :: EnvVarName -> ParseEnv EnvVarValue
askVar name = askOptionalVar name >>= maybe (missingEnvVarError name) pure
  where
    missingEnvVarError = throwError . MissingEnvVarError

askOptionalVar :: EnvVarName -> ParseEnv (Maybe EnvVarValue)
askOptionalVar name =
    ParseEnv . ReaderT $ \env ->
        pure (HashMap.lookup name env)

parseEnv
    :: HashMap EnvVarName EnvVarValue
    -> ParseEnv a
    -> Either ParseEnvError a
parseEnv env (ParseEnv (ReaderT f)) =
    case f env of
        ExceptT (Identity r) -> r

-- }}} Environment Parser -----------------------------------------------------

-- {{{ Application Names ------------------------------------------------------

data AppNames = AppNames
    { exePath :: FilePath
    , exeName :: String
    , usedName :: String
    }

appNames :: IO AppNames
appNames = do
    usedName <- getProgName
    getScriptPath <&> \case
        Executable exePath ->
            AppNames{exePath, exeName = takeFileName exePath, usedName}

        RunGHC exePath ->
            AppNames{exePath, exeName = takeFileName exePath, usedName}

        Interactive ->
            AppNames{exePath = "", exeName = "", usedName}

-- }}} Application Names ------------------------------------------------------
