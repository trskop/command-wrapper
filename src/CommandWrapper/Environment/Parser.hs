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
    , parseEnvIO
    , parseEnvWithIO

    -- ** Parsing Primitives
    , var
    , optionalVar

    -- ** Parsing Primitives: Command Wrapper
    , commandWrapperVar
    , commandWrapperVar'
    , commandWrapperVarName
    )
  where

import Control.Monad ((>=>))
import Control.Monad.IO.Class (MonadIO)
import Data.Either (Either)
import Data.Functor ((<$>), (<&>))

import Control.Monad.Reader (ask)
import Data.HashMap.Strict (HashMap)
import System.Environment.Parser
    ( ParseEnv(..)
    , ParseEnvError(..)
    , optionalVar
    , parseEnvWithIO
    , var
    )
import qualified System.Environment.Parser as Parser (parseEnv)

import CommandWrapper.Environment.Variable
    ( CommandWrapperPrefix
    , CommandWrapperVarName
    , EnvVarName
    , EnvVarValue
    , commandWrapperPrefix
    , getCommandWrapperVarName
    )


commandWrapperVar'
    :: CommandWrapperVarName
    -> ParseEnv CommandWrapperPrefix (EnvVarName, EnvVarValue)
commandWrapperVar' name = do
    varName <- commandWrapperVarName name
    (varName, ) <$> var varName

commandWrapperVar
    :: CommandWrapperVarName
    -> ParseEnv CommandWrapperPrefix EnvVarValue
commandWrapperVar = commandWrapperVarName >=> var

commandWrapperVarName
    :: CommandWrapperVarName
    -> ParseEnv CommandWrapperPrefix EnvVarName
commandWrapperVarName name =
    ask <&> \prefix ->
        getCommandWrapperVarName prefix name

-- | Parse Command Wrapper environment variables using provided parser.  It's a
-- specialised version of 'Parser.parseEnv' from "System.Environment.Parser":
--
-- @
-- 'parseEnv' = 'parseEnv' 'commandWrapperPrefix'
-- @
parseEnv
    :: HashMap EnvVarName EnvVarValue
    -> ParseEnv CommandWrapperPrefix a
    -> Either ParseEnvError a
parseEnv = Parser.parseEnv commandWrapperPrefix

-- | Variant of 'parseEnv' that populates the environment by reading process
-- environment variables.
parseEnvIO
    :: MonadIO io
    => (forall void. ParseEnvError -> io void)
    -> ParseEnv CommandWrapperPrefix a
    -> io a
parseEnvIO = parseEnvWithIO parseEnv
