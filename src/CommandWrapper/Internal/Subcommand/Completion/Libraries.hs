{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
-- |
-- Module:      CommandWrapper.Internal.Subcommand.Completion.DhallLibraries
-- Description: Print requested library to stdout or write it into a file.
-- Copyright:   (c) 2019 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions; POSIX.
--
-- Print requested library to stdout or write it into a file.
module CommandWrapper.Internal.Subcommand.Completion.Libraries
    (
      ImportOrContent(..)

    -- * Bash Library
    , putBashLibrary

    -- * Dhall Libraries
    , DhallLibrary(..)
    , putDhallLibrary

    -- * Completion Script
    )
  where

import Control.Applicative (pure)
import Control.Monad ((>>=))
import Data.Either (Either(Left, Right))
import Data.Monoid ((<>))
import Data.String (fromString)
import GHC.Generics (Generic)
import System.Exit (ExitCode(ExitFailure), exitWith)
import System.IO (IO, IOMode(WriteMode), stderr, stdout, withFile)
import Text.Show (Show, show)

import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString (putStr)
import Data.Either.Validation (validationToEither)
import Data.FileEmbed (embedFile)
import Data.Output
    ( OutputFile(OutputFile)
    , OutputHandle(OutputHandle, OutputNotHandle)
    , OutputStdoutOrFile
    )
import Data.Text (Text)
import qualified Data.Text.IO as Text (hPutStrLn, putStr)
import qualified Data.Text.Prettyprint.Doc as Pretty (Doc)
import qualified Dhall (auto, extract)
import qualified Dhall.TH (staticDhallExpression)
import qualified System.AtomicWrite.Writer.ByteString as ByteString
    ( atomicWriteFile
    )
import qualified System.AtomicWrite.Writer.Text as Text (atomicWriteFile)

import CommandWrapper.Config.Global (Config(Config, colourOutput, verbosity))
import qualified CommandWrapper.Internal.Subcommand.Config.Dhall as Dhall
    ( hPutExpr
    )
import CommandWrapper.Message (errorMsg)


data ImportOrContent = Import | Content
  deriving stock (Generic, Show)

-- TODO: We should consider piping the output to a pager or similar tool when
-- the stdout is attached to a terminal.  For example `bat` would be a great
-- choice if it is installed.  This applies for all functions in here.

putBashLibrary
    :: (forall ann. Pretty.Doc ann)
    -> Config
    -> ImportOrContent
    -> OutputStdoutOrFile
    -> IO ()
putBashLibrary subcmd Config{colourOutput, verbosity} importOrContent = \case
    OutputHandle _ ->
        case importOrContent of
            Import ->
                getLibImportScript >>= Text.putStr
            Content ->
                ByteString.putStr libContent

    OutputNotHandle (OutputFile fn) ->
        case importOrContent of
            Import ->
                getLibImportScript >>= Text.atomicWriteFile fn
            Content ->
                ByteString.atomicWriteFile fn libContent
  where
    libContent :: ByteString
    libContent = $(embedFile "bash/lib.bash")

    libImportScriptExpr =
        $(Dhall.TH.staticDhallExpression
            "./dhall/import-shell-library.dhall < Bash >.Bash"
        )

    getLibImportScript :: IO Text
    getLibImportScript = do
        let script = Dhall.extract Dhall.auto libImportScriptExpr
        case validationToEither script of
            Left e -> do
                -- TODO: Error handling  should probably be left to higher
                -- level code.
                errorMsg subcmd verbosity colourOutput stderr
                    ( "Failed to generate completion script: "
                    <> fromString (show e)
                    )
                -- TODO: This is probably not the best exit code.
                exitWith (ExitFailure 1)

            Right t ->
                pure t

-- {{{ Dhall Libraries --------------------------------------------------------

data DhallLibrary
    = PreludeV11_1_0
    | PreludeV12_0_0
    | CommandWrapper
    | CommandWrapperExec
  deriving stock (Generic, Show)

putDhallLibrary
    :: Config
    -> DhallLibrary
    -> ImportOrContent
    -> OutputStdoutOrFile
    -> IO ()
putDhallLibrary config dhallLib importOrContent = \case
    OutputHandle _ ->
        hPutDhallLibrary stdout

    OutputNotHandle (OutputFile filePath) ->
        -- TODO: Use atomic version instead.
        withFile filePath WriteMode hPutDhallLibrary
  where
    hPutExpr = Dhall.hPutExpr config

    hPutDhallLibrary h = case (dhallLib, importOrContent) of
        (PreludeV11_1_0, Content) -> hPutExpr h
            $(Dhall.TH.staticDhallExpression
                "https://prelude.dhall-lang.org/v12.0.0/package.dhall\
                \ sha256:aea6817682359ae1939f3a15926b84ad5763c24a3740103202d2eaaea4d01f4c"
            )

        (PreludeV11_1_0, Import) -> Text.hPutStrLn h
            "https://prelude.dhall-lang.org/v12.0.0/package.dhall\
            \ sha256:aea6817682359ae1939f3a15926b84ad5763c24a3740103202d2eaaea4d01f4c"

        (PreludeV12_0_0, Content) -> hPutExpr h
            $(Dhall.TH.staticDhallExpression
                "https://prelude.dhall-lang.org/v12.0.0/package.dhall\
                \ sha256:aea6817682359ae1939f3a15926b84ad5763c24a3740103202d2eaaea4d01f4c"
            )

        (PreludeV12_0_0, Import) -> Text.hPutStrLn h
            "https://prelude.dhall-lang.org/v12.0.0/package.dhall\
            \ sha256:aea6817682359ae1939f3a15926b84ad5763c24a3740103202d2eaaea4d01f4c"

        -- IMPORTANT!
        --
        -- Whenever one of these fails to compile then Command
        -- Wrapper library was updated, and both hash and import
        -- need updating.  Unfortunately it won't always fail due
        -- to caching. Keeping this up to date will be a pain.
        --
        -- TODO: Figure out how to do this in a better way.

        (CommandWrapper, Content) -> hPutExpr h
            $(Dhall.TH.staticDhallExpression
                "./dhall/CommandWrapper/package.dhall\
                \ sha256:db7beaa043832c8deca4f19321014f4e0255bc5081dae5ad918d7137711997f5"
            )

        (CommandWrapper, Import) -> Text.hPutStrLn h
            "https://raw.githubusercontent.com/trskop/command-wrapper/ed45bcbf451d7cef902cf046856e4b2529a4505b/dhall/CommandWrapper/package.dhall\
            \ sha256:db7beaa043832c8deca4f19321014f4e0255bc5081dae5ad918d7137711997f5"

        (CommandWrapperExec, Content) -> hPutExpr h
            $(Dhall.TH.staticDhallExpression
                "./dhall/Exec/package.dhall\
                \ sha256:38900a8210a254861364428b9ab96a1ac473a73b2fc271085d4dda2afc2e9a9c"
            )

        (CommandWrapperExec, Import) -> Text.hPutStrLn h
            "https://raw.githubusercontent.com/trskop/exec/ed45bcbf451d7cef902cf046856e4b2529a4505b/dhall/Exec/package.dhall\
            \ sha256:38900a8210a254861364428b9ab96a1ac473a73b2fc271085d4dda2afc2e9a9c"

-- }}} Dhall Libraries --------------------------------------------------------
