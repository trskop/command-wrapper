{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:      CommandWrapper.Environment.Variable
-- Description: TODO: Module synopsis
-- Copyright:   (c) 2018 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- TODO: Module description.
module CommandWrapper.Environment.Variable
    ( EnvVarName
    , EnvVarValue

    -- * Command Wrapper Environment Variables
    , CommandWrapperPrefix
    , CommandWrapperVarName(..)
    , commandWrapperPrefix
    , commandWrapperVarName
    )
  where

import Data.String (String)
import Data.Function ((.))
import Data.Monoid ((<>))
import GHC.Generics (Generic)
import Text.Show (Show)


type EnvVarName = String
type EnvVarValue = String

type CommandWrapperPrefix = String

-- | @'commandWrapperPrefix' = \"COMMAND_WRAPPER\"@
commandWrapperPrefix :: CommandWrapperPrefix
commandWrapperPrefix = "COMMAND_WRAPPER"

-- | Enumeration of variables that are part of Command Wrapper
-- environment\/protocol.
data CommandWrapperVarName
    = CommandWrapperExe
    -- ^
    -- > <prefix>_EXE
    | CommandWrapperName
    -- ^
    -- > <prefix>_NAME
    | CommandWrapperConfig
    -- ^
    -- > <prefix>_CONFIG
    | CommandWrapperVerbosity
    -- ^
    -- > <prefix>_VERBOSITY
    | CommandWrapperColour
    -- ^
    -- > <prefix>_COLOUR
    | CommandWrapperVersion
    -- ^
    -- > <prefix>_VERSION
  deriving (Generic, Show)

-- | Get fully formed Command Wrapper variable name:
--
-- > <prefix>_{EXE|NAME|CONFIG|VERBOSITY|COLOUR}
commandWrapperVarName
    :: CommandWrapperPrefix
    -> CommandWrapperVarName
    -> EnvVarName
commandWrapperVarName prefix = (prefix <>) . \case
    CommandWrapperExe -> "_EXE"
    CommandWrapperName -> "_NAME"
    CommandWrapperConfig -> "_CONFIG"
    CommandWrapperVerbosity -> "_VERBOSITY"
    CommandWrapperColour -> "_COLOUR"
    CommandWrapperVersion -> "_VERSION"
