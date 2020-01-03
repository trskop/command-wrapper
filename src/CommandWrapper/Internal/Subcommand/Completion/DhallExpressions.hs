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

import Data.Monoid (Monoid, (<>))
import Data.String (IsString)


-- {{{ Dhall Prelude Library --------------------------------------------------

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

-- }}} Dhall Prelude Library --------------------------------------------------

-- {{{ Command Wrapper and Exec Libraries -------------------------------------

commit :: IsString s => s
commit = "e3e6ee212c767e868914425e6e9b788c7c45432f"

urlBase :: IsString s => s
urlBase = "https://raw.githubusercontent.com/trskop/command-wrapper/"

commandWrapperHash :: IsString s => s
commandWrapperHash =
    "sha256:db7beaa043832c8deca4f19321014f4e0255bc5081dae5ad918d7137711997f5"

commandWrapperContent :: (IsString s, Monoid s) => s
commandWrapperContent =
    "./dhall/CommandWrapper/package.dhall " <> commandWrapperHash

commandWrapperImport :: (IsString s, Monoid s) => s
commandWrapperImport =
    urlBase <> commit <> "/dhall/CommandWrapper/package.dhall "
    <> commandWrapperHash

execHash :: IsString s => s
execHash =
    "sha256:a076951c08d9235ffd9d29b7414be8d19b1b9615b9f28c0ff086328358bcb5fa"

execContent :: (IsString s, Monoid s) => s
execContent =
    "./dhall/Exec/package.dhall " <> execHash

execImport :: (IsString s, Monoid s) => s
execImport =
    urlBase <> commit <> "/dhall/Exec/package.dhall " <> execHash

-- }}} Command Wrapper and Exec Libraries -------------------------------------

-- {{{ Shell Completion Template ----------------------------------------------

shellCompletionTemplate :: IsString s => s
shellCompletionTemplate = "./dhall/completion.dhall"

-- }}} Shell Completion Template ----------------------------------------------
