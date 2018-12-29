{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:      CommandWrapper.Environment.AppNames
-- Description: TODO: Module synopsis
-- Copyright:   (c) 2018 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- TODO: Module description.
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

import System.Environment.Executable (ScriptPath(..), getScriptPath)
import System.FilePath (takeFileName)


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
--     , 'exeVersion' = ... -- Whatever the first argument of 'getAppNames' returned.
--     , 'usedName' = \"yx\"
--     , 'names' = \"yx\" :| [\"command-wrapper\"]
--     }
-- @
getAppNames :: IO Version -> IO AppNames
getAppNames getVersion = do
    usedName <- getUsedName
    version <- getVersion
    getScriptPath <&> \case
        Executable exePath ->
            appNamesWithExePath usedName exePath version

        RunGHC exePath ->
            appNamesWithExePath usedName exePath version

        Interactive ->
            AppNames
                { exePath = ""
                , exeName = ""
                , exeVersion = makeVersion []
                , usedName
                , names = usedName :| []
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
            }

    getUsedName = do
        -- Environment variable @COMMAND_WRAPPER_INVOKE_AS=<used-name>@
        -- overrides the default value of 'usedName' This is useful for testing
        -- and when subcommands call other subcommands.  With this there is no
        -- need to pass around path to toolset binary/symlink.
        possiblyOverrideProgName <- lookupEnv "COMMAND_WRAPPER_INVOKE_AS"
        maybe getProgName pure possiblyOverrideProgName
