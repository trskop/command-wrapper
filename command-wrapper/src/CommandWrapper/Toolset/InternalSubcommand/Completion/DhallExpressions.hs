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
      preludeV12_0_0Content
    , preludeV12_0_0Import
    , preludeV13_0_0Content
    , preludeV13_0_0Import
    , preludeV14_0_0Content
    , preludeV14_0_0Import
    , preludeV15_0_0Content
    , preludeV15_0_0Import

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

preludeV12_0_0Content :: IsString s => s
preludeV12_0_0Content =
#ifdef DHALL_PRELUDE_V12_0_0
    DHALL_PRELUDE_V12_0_0
#else
    preludeV12_0_0Import
#endif

preludeV12_0_0Import :: IsString s => s
preludeV12_0_0Import =
    "https://prelude.dhall-lang.org/v12.0.0/package.dhall sha256:aea6817682359ae1939f3a15926b84ad5763c24a3740103202d2eaaea4d01f4c"

preludeV13_0_0Content :: IsString s => s
preludeV13_0_0Content =
#ifdef DHALL_PRELUDE_V13_0_0
    DHALL_PRELUDE_V13_0_0
#else
    preludeV13_0_0Import
#endif

preludeV13_0_0Import :: IsString s => s
preludeV13_0_0Import =
    "https://prelude.dhall-lang.org/v13.0.0/package.dhall sha256:4aa8581954f7734d09b7b21fddbf5d8df901a44b54b4ef26ea71db92de0b1a12"

preludeV14_0_0Content :: IsString s => s
preludeV14_0_0Content =
#ifdef DHALL_PRELUDE_V14_0_0
    DHALL_PRELUDE_V14_0_0
#else
    preludeV14_0_0Import
#endif

preludeV14_0_0Import :: IsString s => s
preludeV14_0_0Import =
    "https://prelude.dhall-lang.org/v14.0.0/package.dhall sha256:c1b3fc613aabfb64a9e17f6c0d70fe82016a030beedd79851730993e9083fde2"

preludeV15_0_0Content :: IsString s => s
preludeV15_0_0Content =
#ifdef DHALL_PRELUDE_V15_0_0
    DHALL_PRELUDE_V15_0_0
#else
    preludeV15_0_0Import
#endif

preludeV15_0_0Import :: IsString s => s
preludeV15_0_0Import =
    "https://prelude.dhall-lang.org/v15.0.0/package.dhall sha256:6b90326dc39ab738d7ed87b970ba675c496bed0194071b332840a87261649dcd"

-- }}} Dhall Prelude Library --------------------------------------------------

-- {{{ Command Wrapper and Exec Libraries -------------------------------------

commit :: IsString s => s
commit = "0.1.0.0-rc9"

urlBase :: IsString s => s
urlBase = "https://raw.githubusercontent.com/trskop/command-wrapper/"

commandWrapperHash :: IsString s => s
commandWrapperHash =
    "sha256:6ccf9cb286d33fe60d8d4d4fa2f48f91475207e5c2b51c27950962983db14310"

commandWrapperContent :: (IsString s, Monoid s) => s
commandWrapperContent =
    "./dhall/CommandWrapper/package.dhall " <> commandWrapperHash

commandWrapperImport :: (IsString s, Monoid s) => s
commandWrapperImport =
    urlBase <> commit <> "/command-wrapper/dhall/CommandWrapper/package.dhall "
    <> commandWrapperHash

execHash :: IsString s => s
execHash =
    "sha256:9744c9541ded50fe428b69b637307ae4034c09ce527b4c42ead662cb499f3b4d"

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
