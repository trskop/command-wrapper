{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:      CommandWrapper.Environment.AppNames
-- Description: Representation of all the names and paths under wich Command
--              Wrapper is known.
-- Copyright:   (c) 2018-2019 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Representation of all the names and paths under wich Command Wrapper is
-- known.  These are used to search for subcommands, populate environment
-- variables for subcommands, in error messages, etc.
--
-- See also @command-wrapper(1)@ and @command-wrapper-subcommand-protocol(7)@
-- manual pages.
module CommandWrapper.Environment.AppNames
    (
    -- * Application Names
      AppNames(..)
    , getAppNames
    )
  where

import Control.Applicative (pure)
import Data.Eq ((==))
import Data.Functor ((<&>))
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Maybe (maybe)
import Data.String (String)
import Data.Version (Version, makeVersion)
import GHC.Generics (Generic)
import System.Environment (getProgName, lookupEnv)
import System.IO (FilePath, IO)
import Text.Show (Show)

import qualified Data.Text as Text (unpack)
import System.Environment.Executable (ScriptPath(..), getScriptPath)
import System.FilePath (takeFileName)

import CommandWrapper.Environment.Variable
    ( CommandWrapperPrefix
    , CommandWrapperToolsetVarName(CommandWrapperInvokeAs)
    , getCommandWrapperToolsetVarName
    )


data AppNames = AppNames
    { exePath :: FilePath
    -- ^ Full path to the executable, symlinks are resolved.
    --
    -- For example, default installation path of Command Wrapper for @\"neo\"@
    -- result in:
    --
    -- @
    -- 'exePath' = \"/home/neo/.local/lib/command-wrapper/command-wrapper\"
    -- @

    , exeName :: String
    -- ^ Executable file name
    --
    -- For example, if
    --
    -- @
    -- 'exePath' = \"/home/neo/.local/lib/command-wrapper/command-wrapper\"
    -- @
    --
    -- Then
    --
    -- @
    -- 'exeName' = \"command-wrapper\"
    -- @

    , exeVersion :: Version
    -- ^ Version number of the executable.  How this information is gathered
    -- is intentionally left vague.

    , usedName :: String
    -- ^ Name under which the executable was executed.
    --
    -- For example if user invoked command @yx SOMETHING@, which was resolved
    -- by shell into a symlink:
    --
    -- @
    -- \"/home/neo/bin/yx\" -> \"/home/neo/.local/lib/command-wrapper/command-wrapper\"
    -- @
    --
    -- Then:
    --
    -- @
    -- 'usedName' = \"yx\"
    -- @
    --
    -- This value can be overriden by an environment variable
    -- @<prefix>_INVOKE_AS=<used-name>@. See 'CommandWrapperPrefix',
    -- 'CommandWrapperInvokeAs', and @command-wrapper(1)@ manual page section
    -- /ENVIRONMENT VARIABLES/.

    , names :: NonEmpty String
    -- ^ List of names under which the Command Wrapper executable is known.
    -- Order is significant when searching for a subcommand.  First name has
    -- precedence.
    --
    -- For example if @'exeName' = \"command-wrapper\"@, and
    -- @'usedName' = \"yx\"@ then:
    --
    -- @
    -- 'names' = \"yx\" :| [\"command-wrapper\"]
    -- @

    , commandWrapperPrefix :: CommandWrapperPrefix
    -- ^ Prefix shared by all Command Wrapper environment variables.
    }
  deriving (Generic, Show)

-- | Smart constructor for 'AppNames'.
--
-- Lets say that we have a following installation:
--
-- > ~/
-- > ├── bin/
-- > │   ├── yx -> ../.local/lib/command-wrapper/command-wrapper
-- > │   └── ...
-- > ├── .local/lib/command-wrapper/
-- > │   ├── command-wrapper
-- > │   └── ...
-- > └── ...
--
-- If user @\"neo\"@ invokes @yx SOMETHING@ command and @\"$HOME\/bin\"@ is in
-- their @\"$PATH\"@ then return value of this function would be:
--
-- @
-- 'AppNames'
--     { 'exePath' = \"/home/neo/.local/lib/command-wrapper/command-wrapper\"
--     , 'exeName' = \"command-wrapper\"
--     , 'exeVersion' = ... -- Whatever the second argument of 'getAppNames' returned.
--     , 'usedName' = \"yx\"
--     , 'names' = \"yx\" :| [\"command-wrapper\"]
--     }
-- @
getAppNames
    :: CommandWrapperPrefix
    -- ^ Prefix used by Command Wrapper's environment variables.  Usually
    -- 'CommandWrapper.Environment.Variable.defaultCommandWrapperPrefix'.
    -> IO Version
    -> IO AppNames
getAppNames prefix getVersion = do
    usedName <- getUsedName
    version <- getVersion
    getScriptPath <&> \case
        Executable exePath ->
            appNamesWithExePath usedName exePath version

        RunGHC exePath ->
            appNamesWithExePath usedName exePath version

        Interactive ->
            -- TODO: Command Wrapper is probably not able to function with
            -- following values.  This needs to be tested and maybe it's better
            -- to die with honor then to continue.
            AppNames
                { exePath = ""
                , exeName = ""
                , exeVersion = makeVersion []
                , usedName
                , names = usedName :| []
                , commandWrapperPrefix = prefix
                }
  where
    appNamesWithExePath usedName exePath exeVersion =
        let exeName = takeFileName exePath
        in AppNames
            { exePath
            , exeName
            , exeVersion
            , usedName

            -- More specific name has priority, i.e. user defined toolset has
            -- preference from generic 'command-wrapper' commands.
            , names = usedName :| if exeName == usedName then [] else [exeName]
            , commandWrapperPrefix = prefix
            }

    getUsedName = do
        -- Environment variable @<prefix>_INVOKE_AS=<used-name>@ overrides the
        -- default value of 'usedName'. This is useful for testing, and when
        -- subcommands call other subcommands.  With this there is no need to
        -- pass around path to toolset binary/symlink.
        let invokeAsVarName =
                getCommandWrapperToolsetVarName prefix CommandWrapperInvokeAs

        possiblyOverrideProgName <- lookupEnv (Text.unpack invokeAsVarName)
        maybe getProgName pure possiblyOverrideProgName
