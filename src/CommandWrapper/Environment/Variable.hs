{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module:      CommandWrapper.Environment.Variable
-- Description: Environment variables as they are understood by Command Wrapper.
-- Copyright:   (c) 2018 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Environment variables as they are understood by Command Wrapper.
module CommandWrapper.Environment.Variable
    ( EnvVarName
    , EnvVarValue

    -- * Command Wrapper Environment
    --
    -- | Be aware that we want there to be no overlap between
    -- 'CommandWrapperVarName' and 'CommandWrapperToolsetVarName'.  It will
    -- make environment unambiguous when it comes to their purpose.
    , CommandWrapperPrefix
    , commandWrapperPrefix

    -- ** Command Wrapper Subcommand Environment Variables
    --
    -- | See @command-wrapper-subcommand-protocol(7)@ manual page for details.
    , CommandWrapperVarName(..)
    , getCommandWrapperVarName

    -- ** Command Wrapper (Toolset) Environment Variables
    --
    -- | See @command-wrapper(1)@ manual page section /ENVIRONMENT VARIABLES/
    -- for details.
    , CommandWrapperToolsetVarName(..)
    , getCommandWrapperToolsetVarName
    )
  where

import Data.Function ((.))
import Data.Monoid ((<>))
import GHC.Generics (Generic)
import Text.Show (Show)

import Data.Text (Text)
import System.Environment.Variable (EnvVarName, EnvVarValue)


type CommandWrapperPrefix = Text

-- | @'commandWrapperPrefix' = \"COMMAND_WRAPPER\"@
commandWrapperPrefix :: CommandWrapperPrefix
commandWrapperPrefix = "COMMAND_WRAPPER"

-- | Enumeration of variables that are part of Command Wrapper subcommand
-- environment\/protocol.
data CommandWrapperVarName
    = CommandWrapperExe
    -- ^ Full path to Command Wrapper executable, all symbolic links should be
    -- resolved.  In other words it's the path under which the executable is
    -- physically available.
    --
    -- > <prefix>_EXE
    --
    -- See also module "CommandWrapper.Environment.AppNames", especially
    -- 'CommandWrapper.Environment.AppNames.exePath'.

    | CommandWrapperName
    -- ^ Name under which the executable was executed.
    --
    -- > <prefix>_NAME
    --
    -- See also module "CommandWrapper.Environment.AppNames", especially
    -- 'CommandWrapper.Environment.AppNames.usedName'.

    | CommandWrapperSubcommand
    -- ^
    -- > <prefix>_SUBCOMMAND
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
  deriving stock (Generic, Show)

-- | Get fully formed Command Wrapper variable name:
--
-- > <prefix>_{EXE|NAME|SUBCOMMAND|CONFIG|VERBOSITY|COLOUR}
getCommandWrapperVarName
    :: CommandWrapperPrefix
    -> CommandWrapperVarName
    -> EnvVarName
getCommandWrapperVarName prefix = (prefix <>) . \case
    CommandWrapperExe -> "_EXE"
    CommandWrapperName -> "_NAME"
    CommandWrapperSubcommand -> "_SUBCOMMAND"
    CommandWrapperConfig -> "_CONFIG"
    CommandWrapperVerbosity -> "_VERBOSITY"
    CommandWrapperColour -> "_COLOUR"
    CommandWrapperVersion -> "_VERSION"

-- | Enumeration of variables that are used\/interpreted by the top-level
-- (toolset) Command Wrapper executable.
data CommandWrapperToolsetVarName
    = CommandWrapperInvokeAs
    -- ^ Overrides the the name under which the (toolset) Command Wrapper
    -- executable was executed. This is useful for testing, and when
    -- subcommands call other subcommands.  With this there is no need to pass
    -- around path to toolset binary\/symlink to individual toolset, only to
    -- final Command Wrapper executable.
    --
    -- > <prefix>_INVOKE_AS
    --
    -- See also module "CommandWrapper.Environment.AppNames", especially
    -- 'CommandWrapper.Environment.AppNames.getAppNames' function.
  deriving stock (Generic, Show)

-- | Get fully formed Command Wrapper (toolset) variable name:
--
-- > <prefix>_INVOKE_AS
getCommandWrapperToolsetVarName
    :: CommandWrapperPrefix
    -> CommandWrapperToolsetVarName
    -> EnvVarName
getCommandWrapperToolsetVarName prefix = (prefix <>) . \case
    CommandWrapperInvokeAs -> "_INVOKE_AS"
