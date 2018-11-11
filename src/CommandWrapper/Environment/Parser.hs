{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TupleSections #-}
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

    -- ** Evaluate Environment Parser
    , parseEnv
    , parseEnvFor
    , parseEnvIO
    , parseEnvForIO
    , parseEnvWithIO

    -- ** Parsing Primitives
    , askVar
    , askOptionalVar

    -- ** Parsing Primitives: Command Wrapper
    , askCommandWrapperVar
    , askCommandWrapperVar'
    , askCommandWrapperVarName
    , askCommandWrapperPrefix
    )
  where

import Control.Applicative (Alternative, Applicative, pure)
import Control.Monad (Monad, MonadPlus, (>=>), (>>=))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Fail (MonadFail(fail))
import Data.Either (Either, either)
import Data.Function (($), (.))
import Data.Functor (Functor, (<$>), (<&>))
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

import CommandWrapper.Environment.Variable
    ( CommandWrapperPrefix
    , CommandWrapperVarName
    , EnvVarName
    , EnvVarValue
    , commandWrapperPrefix
    , commandWrapperVarName
    )


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
        :: ReaderT
            (HashMap EnvVarName EnvVarValue, String)
            (Except ParseEnvError)
            a
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
    ParseEnv . ReaderT $ \(env, _) ->
        pure (HashMap.lookup name env)

askCommandWrapperPrefix :: ParseEnv CommandWrapperPrefix
askCommandWrapperPrefix = ParseEnv . ReaderT $ \(_, prefix) ->
    pure prefix

askCommandWrapperVar'
    :: CommandWrapperVarName
    -> ParseEnv (EnvVarName, EnvVarValue)
askCommandWrapperVar' name = do
    varName <- askCommandWrapperVarName name
    (varName, ) <$> askVar varName

askCommandWrapperVar :: CommandWrapperVarName -> ParseEnv EnvVarValue
askCommandWrapperVar = askCommandWrapperVarName >=> askVar

askCommandWrapperVarName :: CommandWrapperVarName -> ParseEnv EnvVarName
askCommandWrapperVarName name =
    askCommandWrapperPrefix <&> \prefix ->
        commandWrapperVarName prefix name

parseEnvFor
    :: CommandWrapperPrefix
    -> HashMap EnvVarName EnvVarValue
    -> ParseEnv a
    -> Either ParseEnvError a
parseEnvFor prefix env (ParseEnv (ReaderT f)) =
    case f (env, prefix) of
        ExceptT (Identity r) -> r

-- | Parse Command Wrapper environment variables using provided parser.  It's a
-- specialised version of 'parseEnvFor':
--
-- @
-- 'parseEnv' = 'parseEnvFor' 'commandWrapperPrefix'
-- @
parseEnv
    :: HashMap EnvVarName EnvVarValue
    -> ParseEnv a
    -> Either ParseEnvError a
parseEnv = parseEnvFor commandWrapperPrefix

-- | Variant of 'parseEnv' that populates the environment by reading process
-- environment variables.
parseEnvIO
    :: MonadIO io
    => (forall void. ParseEnvError -> io void)
    -> ParseEnv a
    -> io a
parseEnvIO = parseEnvWithIO parseEnv

-- | Generalised version of 'parseEnvIO' that is not bound to 'ParseEnv'
-- implementation.
parseEnvWithIO
    :: MonadIO io
    => (HashMap EnvVarName EnvVarValue -> parser -> Either err a)
    -> (forall void. err -> io void)
    -- ^ Handle parsing error.
    -> parser
    -> io a
parseEnvWithIO f onError parser = do
    env <- HashMap.fromList <$> liftIO getEnvironment
    either onError pure (f env parser)

-- | Variant of 'parseEnvFor' that populates the environment by reading process
-- environment variables.
parseEnvForIO
    :: MonadIO io
    => CommandWrapperPrefix
    -- ^ Prefix of Command Wrapper environment variables.
    -> (forall void. ParseEnvError -> io void)
    -> ParseEnv a
    -> io a
parseEnvForIO prefix = parseEnvWithIO (parseEnvFor prefix)
