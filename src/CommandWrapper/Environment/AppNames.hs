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
    , exeName :: String
    , exeVersion :: Version
    , usedName :: String
    , names :: NonEmpty String
    }
  deriving (Generic, Show)

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
