{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:      CommandWrapper.Internal.Subcommand.Completion.LibraryImports
-- Description: Static Dhall expressions to satisfy TH stage restriction.
-- Copyright:   (c) 2019 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Static Dhall expressions to satisfy TH stage restriction.
module CommandWrapper.Internal.Subcommand.Completion.DhallExpressions
    (
    -- * Dhall Prelude Library
      preludeV11_1_0Content
    , preludeV11_1_0Import
    , preludeV12_0_0Content
    , preludeV12_0_0Import

    -- * Command Wrapper Dhall Library
    , commandWrapperContent
    , commandWrapperImport

    -- * Exec Dhall Library
    , execContent
    , execImport

    -- * Shell Completion Script Template
    , shellCompletionTemplate
    )
  where

import Data.String (IsString)


preludeV11_1_0Content :: IsString s => s
preludeV11_1_0Content = preludeV11_1_0Import

preludeV11_1_0Import :: IsString s => s
preludeV11_1_0Import =
    "https://prelude.dhall-lang.org/v11.1.0/package.dhall\
    \ sha256:99462c205117931c0919f155a6046aec140c70fb8876d208c7c77027ab19c2fa"

preludeV12_0_0Content :: IsString s => s
preludeV12_0_0Content = preludeV12_0_0Import

preludeV12_0_0Import :: IsString s => s
preludeV12_0_0Import =
    "https://prelude.dhall-lang.org/v12.0.0/package.dhall\
    \ sha256:aea6817682359ae1939f3a15926b84ad5763c24a3740103202d2eaaea4d01f4c"

commandWrapperContent :: IsString s => s
commandWrapperContent =
    "./dhall/CommandWrapper/package.dhall\
    \ sha256:db7beaa043832c8deca4f19321014f4e0255bc5081dae5ad918d7137711997f5"

commandWrapperImport :: IsString s => s
commandWrapperImport =
    "https://raw.githubusercontent.com/trskop/command-wrapper/d9109e5df506d180c8491861b8e7d23d98e863d6/dhall/CommandWrapper/package.dhall\
    \ sha256:db7beaa043832c8deca4f19321014f4e0255bc5081dae5ad918d7137711997f5"

execContent :: IsString s => s
execContent =
    "./dhall/Exec/package.dhall\
    \ sha256:d66328a55da4aa3ee070f11e52e839152ed4d270b41fe12060c59ddbc42fbf6b"

execImport :: IsString s => s
execImport =
    "https://raw.githubusercontent.com/trskop/exec/d9109e5df506d180c8491861b8e7d23d98e863d6/dhall/Exec/package.dhall\
    \ sha256:d66328a55da4aa3ee070f11e52e839152ed4d270b41fe12060c59ddbc42fbf6b"

shellCompletionTemplate :: IsString s => s
shellCompletionTemplate = "./dhall/completion.dhall"
