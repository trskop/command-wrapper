{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:      CommandWrapper.Internal.Subcommand.Completion.LibraryImports
-- Description: Static Dhall expressions to satisfy TH stage restriction.
-- Copyright:   (c) 2019-2020 Peter Trško
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
commit = "728a334be750c129603ed35a9a22447576adefe9"

urlBase :: IsString s => s
urlBase = "https://raw.githubusercontent.com/trskop/command-wrapper/"

commandWrapperHash :: IsString s => s
commandWrapperHash =
    "sha256:f5d82869a56a5f01f2b2e6c28fe89202938db14e9770484633d014761e438aba"

commandWrapperContent :: (IsString s, Monoid s) => s
commandWrapperContent =
    "./dhall/CommandWrapper/package.dhall " <> commandWrapperHash

commandWrapperImport :: (IsString s, Monoid s) => s
commandWrapperImport =
    urlBase <> commit <> "/dhall/CommandWrapper/package.dhall "
    <> commandWrapperHash

execHash :: IsString s => s
execHash =
    "sha256:ee2a4c35889b685ee46a10ce1516e2037091839d3ebd53c0b8a8c4a128bf517a"

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
