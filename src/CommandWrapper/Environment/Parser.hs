{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Rank2Types #-}
-- |
-- Module:      CommandWrapper.Environment.Parser
-- Description: TODO: Module synopsis
-- Copyright:   (c) 2018 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- TODO: Module description.
module CommandWrapper.Environment.Parser
    (
    -- * Environment Parser
      ParseEnv(..)
    , ParseEnvError(..)
    , parseEnv
    , parseEnvIO
    , askVar
    , askOptionalVar
    )
  where

import Control.Applicative (Alternative, Applicative, pure)
import Control.Monad (Monad, MonadPlus, (>>=))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Fail (MonadFail(fail))
import Data.Either (Either, either)
import Data.Function (($), (.))
import Data.Functor (Functor, (<$>))
import Data.Functor.Identity (Identity(Identity))
import Data.String (String)
import Data.Semigroup (Semigroup((<>)))
import Data.Maybe (Maybe, maybe)
import Data.Monoid (Monoid(mempty))
import GHC.Generics (Generic)
import System.Environment (getEnvironment)
import Text.Show (Show)

import Control.Monad.Reader (ReaderT(ReaderT))
import Control.Monad.Except (Except, ExceptT(ExceptT), MonadError, throwError)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap (fromList, lookup)

import CommandWrapper.Environment.Variable (EnvVarName, EnvVarValue)


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
