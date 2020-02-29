-- |
-- Module:      $Header$
-- Description: Parser for environment variables.
-- Copyright:   (c) 2018-2020 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Parser for environment variables.
module CommandWrapper.Core.Environment.Parser
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
    , commandWrapperToolsetVarName
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

import CommandWrapper.Core.Environment.Variable
    ( CommandWrapperPrefix
    , CommandWrapperVarName
    , CommandWrapperToolsetVarName
    , EnvVarName
    , EnvVarValue
    , defaultCommandWrapperPrefix
    , getCommandWrapperVarName
    , getCommandWrapperToolsetVarName
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

commandWrapperToolsetVarName
    :: CommandWrapperToolsetVarName
    -> ParseEnv CommandWrapperPrefix EnvVarName
commandWrapperToolsetVarName name =
    ask <&> \prefix ->
        getCommandWrapperToolsetVarName prefix name

-- | Parse Command Wrapper environment variables using provided parser.  It's a
-- specialised version of 'Parser.parseEnv' from "System.Environment.Parser":
--
-- @
-- 'parseEnv' = 'parseEnv' 'defaultCommandWrapperPrefix'
-- @
parseEnv
    :: HashMap EnvVarName EnvVarValue
    -> ParseEnv CommandWrapperPrefix a
    -> Either ParseEnvError a
parseEnv = Parser.parseEnv defaultCommandWrapperPrefix
    -- TODO: Get rid of hardcoded 'defaultCommandWrapperPrefix' at some point.

-- | Variant of 'parseEnv' that populates the environment by reading process
-- environment variables.
parseEnvIO
    :: MonadIO io
    => (forall void. ParseEnvError -> io void)
    -> ParseEnv CommandWrapperPrefix a
    -> io a
parseEnvIO = parseEnvWithIO parseEnv
    -- TODO: Get rid of hardcoded 'defaultCommandWrapperPrefix' (in 'parseEnv')
    -- at some point.
