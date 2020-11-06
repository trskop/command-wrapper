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
      preludeV17_0_0Hash
    , preludeV17_1_0Hash
    , preludeV18_0_0Hash
    , preludeV19_0_0Hash
    , mkPreludeImport
    , mkPreludeRemoteImport

    -- * Command Wrapper Dhall Library
    , commandWrapperContent
    , commandWrapperRemoteImport

    -- * Exec Dhall Library
    , execContent
    , execRemoteImport

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

preludeV17_0_0Hash :: IsString s => s
preludeV17_0_0Hash =
    "sha256:10db3c919c25e9046833df897a8ffe2701dc390fa0893d958c3430524be5a43e"

preludeV17_1_0Hash :: IsString s => s
preludeV17_1_0Hash = preludeV17_0_0Hash
    -- Yes, it's the same hash as v17.0.0 has.

preludeV18_0_0Hash :: IsString s => s
preludeV18_0_0Hash =
    "sha256:2086c6a2e7ce8ee371858036ab60d24d045969defadb4bc9d87639921ec2e028"

preludeV19_0_0Hash :: IsString s => s
preludeV19_0_0Hash =
    "sha256:eb693342eb769f782174157eba9b5924cf8ac6793897fc36a31ccbd6f56dafe2"

mkPreludeImport :: (IsString s, Monoid s) => s -> s -> s
mkPreludeImport version hash =
#ifdef NIX_DHALL_PRELUDE_DIR
    NIX_DHALL_PRELUDE_DIR <> "/v" <> version <> "/package.dhall " <> hash
#else
    mkPreludeRemoteImport version hash
#endif

mkPreludeRemoteImport :: (IsString s, Monoid s) => s -> s -> s
mkPreludeRemoteImport version hash =
    "https://prelude.dhall-lang.org/v" <> version <> "/package.dhall " <> hash

-- }}} Dhall Prelude Library --------------------------------------------------

-- {{{ Command Wrapper and Exec Libraries -------------------------------------

commit :: IsString s => s
commit = "0.1.0.0-rc10"

urlBase :: IsString s => s
urlBase = "https://raw.githubusercontent.com/trskop/command-wrapper/"

commandWrapperHash :: IsString s => s
commandWrapperHash =
    "sha256:0aa3bb490f6dfe88f082663fba4fb60246db5f290356e10cd6babc3d50376615"

commandWrapperContent :: (IsString s, Monoid s) => s
commandWrapperContent =
    "./dhall/CommandWrapper/package.dhall " <> commandWrapperHash

commandWrapperRemoteImport :: (IsString s, Monoid s) => s
commandWrapperRemoteImport =
    urlBase <> commit <> "/command-wrapper/dhall/CommandWrapper/package.dhall "
    <> commandWrapperHash

execHash :: IsString s => s
execHash =
    "sha256:f9c440905b5587d34ce81e0dcb2edb495e5630fc489bbec76cacf8fb6cd6d3d0"

execContent :: (IsString s, Monoid s) => s
execContent =
    "./dhall/Exec/package.dhall " <> execHash

execRemoteImport :: (IsString s, Monoid s) => s
execRemoteImport =
    urlBase <> commit <> "/command-wrapper/dhall/Exec/package.dhall "
    <> execHash

-- }}} Command Wrapper and Exec Libraries -------------------------------------

-- {{{ Shell Completion Template ----------------------------------------------

shellCompletionTemplate :: IsString s => s
shellCompletionTemplate = "./dhall/completion.dhall"

-- }}} Shell Completion Template ----------------------------------------------
