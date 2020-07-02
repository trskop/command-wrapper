{-# LANGUAGE CPP #-}
-- |
-- Module:      $Header$
-- Description: Static Dhall expressions to satisfy TH stage restriction.
-- Copyright:   (c) 2019-2020 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Static Dhall expressions to satisfy TH stage restriction.
module CommandWrapper.Toolset.InternalSubcommand.Completion.DhallExpressions
    (
    -- * Dhall Prelude Library
      preludeV17_0_0Content
    , preludeV17_0_0Import

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
--
-- The reason for having two definitions, one content and the other import, is
-- that in Nix we will need to avoid fetching stuff from the internet,
-- therefore content will have to be provided in a different way.

preludeV17_0_0Content :: IsString s => s
preludeV17_0_0Content =
#ifdef DHALL_PRELUDE_V17_0_0
    DHALL_PRELUDE_V17_0_0
#else
    preludeV17_0_0Import
#endif

preludeV17_0_0Import :: IsString s => s
preludeV17_0_0Import =
    "https://prelude.dhall-lang.org/v17.0.0/package.dhall sha256:10db3c919c25e9046833df897a8ffe2701dc390fa0893d958c3430524be5a43e"

-- }}} Dhall Prelude Library --------------------------------------------------

-- {{{ Command Wrapper and Exec Libraries -------------------------------------

commit :: IsString s => s
commit = "0.1.0.0-rc9"

urlBase :: IsString s => s
urlBase = "https://raw.githubusercontent.com/trskop/command-wrapper/"

commandWrapperHash :: IsString s => s
commandWrapperHash =
    "sha256:d2c55c3c5b21c794b14f353463ede9c704e6168db45461c011efb5a65f75a9ff"

commandWrapperContent :: (IsString s, Monoid s) => s
commandWrapperContent =
    "./dhall/CommandWrapper/package.dhall " <> commandWrapperHash

commandWrapperImport :: (IsString s, Monoid s) => s
commandWrapperImport =
    urlBase <> commit <> "/command-wrapper/dhall/CommandWrapper/package.dhall "
    <> commandWrapperHash

execHash :: IsString s => s
execHash =
    "sha256:a8ed7ac9c1168602dbabd7f60dd9423c01e138122ef2b8b694a3ae1c496f7e7d"

execContent :: (IsString s, Monoid s) => s
execContent =
    "./dhall/Exec/package.dhall " <> execHash

execImport :: (IsString s, Monoid s) => s
execImport =
    urlBase <> commit <> "/command-wrapper/dhall/Exec/package.dhall " <> execHash

-- }}} Command Wrapper and Exec Libraries -------------------------------------

-- {{{ Shell Completion Template ----------------------------------------------

shellCompletionTemplate :: IsString s => s
shellCompletionTemplate = "./dhall/completion.dhall"

-- }}} Shell Completion Template ----------------------------------------------
