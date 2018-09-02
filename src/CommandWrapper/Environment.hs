{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Rank2Types #-}
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
    , mkEnvVars
    , getEnv
    , commandWrapperEnv

    -- * Environment Parser
    , ParseEnv(..)
    , ParseEnvError(..)
    , parseEnv
    , parseEnvIO
    , askVar
    , askOptionalVar

    -- * Application Names
    , AppNames(..)
    , getAppNames
    )
  where

import Prelude

import Control.Applicative (Alternative, Applicative, pure)
import Control.Monad (Monad, MonadPlus, (>>=))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Fail (MonadFail(fail))
import qualified Data.Char as Char (toLower)
import Data.Either (Either)
import Data.Function (($), (.), const)
import Data.Functor (Functor, (<&>))
import Data.Functor.Identity (Identity(Identity))
import Data.String (String)
import Data.Semigroup (Semigroup((<>)))
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Maybe (Maybe, maybe)
import Data.Monoid (Monoid(mempty))
import GHC.Generics (Generic)
import System.Environment (getEnvironment, getProgName)
import System.IO (FilePath)
import Text.Show (Show, show)

import Control.Monad.Reader (ReaderT(ReaderT))
import Control.Monad.Except (Except, ExceptT(ExceptT), MonadError, throwError)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap (fromList, lookup, toList)
import Data.Verbosity (Verbosity)
import System.Environment.Executable (ScriptPath(..), getScriptPath)
import System.FilePath (takeFileName)


type EnvVarName = String
type EnvVarValue = String

-- {{{ Environment Builder ----------------------------------------------------

newtype EnvVars = EnvVars
    { getEnvVars :: String -> HashMap EnvVarName EnvVarValue
    }
  deriving (Generic)

instance Semigroup EnvVars where
    EnvVars f <> EnvVars g = EnvVars $ \prefix ->
        f prefix <> g prefix

instance Monoid EnvVars where
    mempty = EnvVars (const mempty)

data Params = Params
    { exePath :: FilePath
    , name :: FilePath
    , config :: FilePath
    , verbosity :: Verbosity
    }
  deriving (Generic, Show)

mkEnvVars :: Params -> EnvVars
mkEnvVars Params{..} = EnvVars $ \prefix ->
    HashMap.fromList
      [ (prefix <> "_EXE", exePath)
      , (prefix <> "_NAME", name)
      , (prefix <> "_CONFIG", config)
      , (prefix <> "_VERBOSITY", Char.toLower <$> show verbosity)
      ]

getEnv :: MonadIO io => io EnvVars
getEnv = EnvVars . const . HashMap.fromList <$>  liftIO getEnvironment

commandWrapperEnv :: EnvVars -> [(EnvVarName, EnvVarValue)]
commandWrapperEnv (EnvVars mkEnv) = HashMap.toList (mkEnv "COMMAND_WRAPPER")

-- }}} Environment Builder ----------------------------------------------------

-- {{{ Environment Parser -----------------------------------------------------

data ParseEnvError
    = ParseEnvError EnvVarName String
    | MissingEnvVarError EnvVarName
    | ErrorMessage String
    | UnknownError
  deriving (Generic, Show)

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

-- | Variant of 'parseEnvIO' that populates the environment by reading process
-- environment variables.
parseEnvIO
    :: MonadIO io
    => (forall void. ParseEnvError -> io void)
    -> ParseEnv a
    -> io a
parseEnvIO onError parser = do
    env <- HashMap.fromList <$> liftIO getEnvironment
    either onError pure (parseEnv env parser)

-- }}} Environment Parser -----------------------------------------------------

-- {{{ Application Names ------------------------------------------------------

data AppNames = AppNames
    { exePath :: FilePath
    , exeName :: String
    , usedName :: String
    , names :: NonEmpty String
    }
  deriving (Generic, Show)

getAppNames :: IO AppNames
getAppNames = do
    usedName <- getProgName
    getScriptPath <&> \case
        Executable exePath ->
            appNamesWithExePath usedName exePath

        RunGHC exePath ->
            appNamesWithExePath usedName exePath

        Interactive ->
            AppNames
                { exePath = ""
                , exeName = ""
                , usedName
                , names = usedName :| []
                }
  where
    appNamesWithExePath usedName exePath =
        let exeName = takeFileName exePath
        in AppNames
            { exePath
            , exeName
            , usedName

            -- More specific name has priority, i.e. user defined toolset has
            -- preference from generic 'command-wrapper' commands.
            , names = usedName :| if exeName == usedName then [] else [exeName]
            }

-- }}} Application Names ------------------------------------------------------
