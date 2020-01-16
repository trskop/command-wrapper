{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:      CommandWrapper.Config.Global
-- Description: Global toolset configuration.
-- Copyright:   (c) 2018-2020 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Global toolset configuration, i.e. one used by @command-wrapper@ executable.
module CommandWrapper.Config.Global
    (
    -- | Global configuration combines values from following sources:
    --
    -- * Configuration files, see "CommandWrapper.Config.File" module.
    --
    -- * Environment variables, see enum
    --   'CommandWrapper.Environment.Variable.CommandWrapperToolsetVarName' and
    --   'CommandWrapper.Options.ColourOutput.noColorEnvVar'.
    --
    -- * Command line options.
    --
    -- * Default values, see 'def' smart constructor.
    --
    -- Orthogonal to (global) 'Config' there is
    -- 'CommandWrapper.Environment.AppNames.AppNames' data type, that
    -- represents names and paths under which command wrapper is
    -- known\/invoked.
      Config(..)
    , getAliases

    -- * Smart Constructor
    , SearchPath(..)
    , ManPath(..)
    , def

    -- * Configuration Paths
    --
    -- | Base directories where configuration files are looked up.
    , ConfigPaths(..)
    , defConfigPaths
    )
  where

import Data.Bool (Bool(False))
import Data.Maybe (Maybe(Nothing))
import Data.String (String)
import GHC.Generics (Generic)
import System.IO (FilePath)
import Text.Show (Show)

import Data.Verbosity (Verbosity)
import qualified Data.Verbosity as Verbosity (Verbosity(Normal))
import Data.Verbosity.Class (HasVerbosity)
import Dhall (FromDhall)

import CommandWrapper.Core.Config.Alias (Alias)
import CommandWrapper.Core.Config.ColourOutput (ColourOutput)


-- | Represents values from global\/toolset configuration file and global
-- command line options.  Global\/toolset onfiguration file is a subset of
-- these, see "CommandWrapper.Config.File" for details.
data Config = Config
    { aliases :: [Alias]
    -- ^ Subcommand aliases.  These can be used to invoke subcommand in
    -- the form:
    --
    -- > ALIAS [EXTRA_ARGUMENTS]
    --
    -- Which is then translated into:
    --
    -- > COMMAND [ARGUMENTS] [EXTRA_ARGUMENTS]
    --
    -- Don't access 'aliases' directly unless you really don't want to respect
    -- 'ignoreAliases' value.  See 'Alias' data type for more details.

    , searchPath :: [FilePath]
    -- ^ Path where to search for subcommands.

    , manPath :: [FilePath]
    -- ^ Path where to search for manual pages.

    , description :: Maybe String
    -- ^ Description of the toolset command printed as part of help message.

    , extraHelpMessage :: Maybe String
    -- ^ Extra text to be displayed when at the end of the help message.  It is
    -- useful for providing important examples, and references to additional
    -- documentation.

    , verbosity :: Verbosity

    , colourOutput :: ColourOutput
    -- ^ Specifies when output will be colourised.  See 'ColourOutput' data
    -- type for details.  Default value is not defined by 'def' smart
    -- constructor to allow various overrides.

    , ignoreAliases :: Bool
    -- ^ When set to 'True' 'getAliases' will return an empty list instead of
    -- the value of 'aliases'.  See 'aliases' and 'getAliases' for more
    -- information.

    , changeDirectory :: Maybe FilePath
    -- ^ Change directory before current working directory, if specified,
    -- before doing anything.

    , configPaths :: ConfigPaths
    -- ^ Set of directories that are used to search for configuration files.
    -- See 'ConfigPaths' for more details.
    }
  deriving stock (Generic, Show)
  deriving anyclass (FromDhall, HasVerbosity)

-- | Used by 'def' smart constructor to avoid accidentally swapping 'manPath'
-- and 'searchPath'..
newtype SearchPath = SearchPath [FilePath]
  deriving stock (Generic, Show)

-- | Used by 'def' smart constructor to avoid accidentally swapping 'manPath'
-- and 'searchPath'..
newtype ManPath = ManPath [FilePath]
  deriving stock (Generic, Show)

-- | Smart constructor for 'Config'.  'ColourOutput', 'SearchPath', and
-- 'ManPath' can be overriden by environment variables, therefore they are
-- taken as arguments.
--
-- Defaults:
--
-- * 'aliases' -- No aliases, i.e. empty list @[]@.
-- * 'changeDirectory' -- No, i.e. 'Nothing'.
-- * 'description' -- None, i.e. 'Nothing'.
-- * 'extraHelpMessage' -- None, i.e. 'Nothing'.
-- * 'ignoreAliases' -- No, i.e. 'False'.
-- * 'verbosity' -- 'Verbosity.Normal'.
def :: ColourOutput
    -- ^ Default value of 'ColourOutput' is affected by environment variables,
    -- e.g. @NO_COLOR@.
    -> SearchPath
    -> ManPath
    -> ConfigPaths
    -> Config
def colourOutput (SearchPath searchPath) (ManPath manPath) configPaths = Config
    { aliases = []
    , changeDirectory = Nothing
    , colourOutput
    , description = Nothing
    , extraHelpMessage = Nothing
    , ignoreAliases = False
    , manPath
    , searchPath
    , verbosity = Verbosity.Normal
    , configPaths
    }

-- | Lookup aliases from 'Config', respects 'ignoreAliases' option.  Use this
-- instead of accessing aliases directly.
getAliases :: Config -> [Alias]
getAliases Config{aliases, ignoreAliases} =
    if ignoreAliases
        then []
        else aliases

-- | Configuration base directories where configuration is looked up.  The
-- lookup order for toolset configuration is:
--
-- 1. @${system}\/command-wrapper\/default.dhall
-- 2. @${user}\/command-wrapper\/default.dhall
-- 3. @${local}\/command-wrapper\/default.dhall
-- 4. @${system}\/${toolset}\/default.dhall
-- 5. @${user}\/${toolset}\/default.dhall
-- 6. @${local}\/${toolset}\/default.dhall
data ConfigPaths = ConfigPaths
    { system :: Maybe FilePath
    , user :: FilePath
    , local :: Maybe FilePath
    }
  deriving stock (Generic, Show)
  deriving anyclass (FromDhall)

-- | Smart constructor for default\/initial value of 'ConfigPaths'.
defConfigPaths
    :: FilePath
    -- ^ User config directory is mandatory.
    -> ConfigPaths
defConfigPaths user = ConfigPaths
    { system = Nothing
    , user
    , local = Nothing
    }
