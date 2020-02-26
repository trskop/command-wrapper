{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
-- |
-- Module:      CommandWrapper.Internal.Subcommand.Completion.DhallLibraries
-- Description: Print requested library to stdout or write it into a file.
-- Copyright:   (c) 2019-2020 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions; POSIX.
--
-- Print requested library to stdout or write it into a file.
module CommandWrapper.Internal.Subcommand.Completion.Libraries
    (
    -- * Script and Dhall Libraries
      LibraryOptions(..)
    , Library(..)
    , ImportOrContent(..)
    , defLibraryOptions
    , putLibrary

    -- ** Bash Library
    , putBashLibrary

    -- ** Dhall Libraries
    , DhallLibrary(..)
    , parseDhallLibrary
    , showDhallLibrary
    , putDhallLibrary

    -- * Shell Completion Script
    , ScriptOptions(..)
    , defScriptOptions
    , putShellCompletionScript
    )
  where

import Prelude (Bounded, Enum)

import Control.Applicative (pure)
import Control.Monad ((>>=))
import Data.Either (Either(Left, Right))
import Data.Eq (Eq)
import Data.Functor ((<$>))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Monoid (Endo(Endo, appEndo), (<>))
import Data.String (IsString, String, fromString)
import Data.Void (Void)
import GHC.Generics (Generic)
import System.Exit (ExitCode(ExitFailure), exitWith)
import System.IO (IO, IOMode(WriteMode), stderr, stdout, withFile)
import Text.Show (Show, show)

import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString (putStr)
import Data.Either.Validation (validationToEither)
import Data.FileEmbed (embedFile)
import Data.Generics.Product.Typed (typed)
import Data.Output
    ( HasOutput(Output)
    , OutputFile(OutputFile)
    , OutputHandle(OutputHandle, OutputNotHandle)
    , OutputStdoutOrFile
    , pattern OutputStdoutOnly
    )
import qualified Data.Output (HasOutput(output))
import Data.Text (Text)
import qualified Data.Text.IO as Text (hPutStrLn, putStr)
import qualified Data.Text.Prettyprint.Doc as Pretty (Doc)
import qualified Dhall (ExtractErrors, auto, extract)
import qualified Dhall.Src as Dhall (Src)
import qualified Dhall.TH (staticDhallExpression)
import qualified System.AtomicWrite.Writer.ByteString as ByteString
    ( atomicWriteFile
    )
import qualified System.AtomicWrite.Writer.Text as Text (atomicWriteFile)

import CommandWrapper.Config.Global (Config(Config, colourOutput, verbosity))
import CommandWrapper.Core.Config.Shell
    ( HasShell(..)
    , Shell(..)
    )
import CommandWrapper.Core.Environment (AppNames(exePath, usedName))
import qualified CommandWrapper.Internal.Subcommand.Config.Dhall as Dhall
    ( hPutExpr
    )
import CommandWrapper.Internal.Subcommand.Completion.DhallExpressions
    ( commandWrapperContent
    , commandWrapperImport
    , execContent
    , execImport
    , preludeV11_1_0Content
    , preludeV11_1_0Import
    , preludeV12_0_0Content
    , preludeV12_0_0Import
    , preludeV13_0_0Content
    , preludeV13_0_0Import
    , preludeV14_0_0Content
    , preludeV14_0_0Import
    , shellCompletionTemplate
    )
import CommandWrapper.Core.Message (errorMsg)


-- TODO:
--
-- *   We should consider piping the output to a pager or similar tool when the
--     stdout is attached to a terminal.  For example `bat` would be a great
--     choice if it is installed.  This applies for all functions in here.

-- {{{ Script and Dhall Libraries ---------------------------------------------

data LibraryOptions = LibraryOptions
    { library :: Library
    -- ^ Which library or import to produce.
    , output :: OutputStdoutOrFile
    -- ^ Where to write the library or import to.
    , importOrContent :: ImportOrContent
    -- ^ Produce library content or import statement.
    }
  deriving stock (Generic, Show)

instance HasShell LibraryOptions where
    updateShell f = Endo \opts@LibraryOptions{library} ->
        (opts :: LibraryOptions)
            { library = updateShell f `appEndo` library
            }

instance HasOutput LibraryOptions where
    type Output LibraryOptions = OutputStdoutOrFile
    output = typed

defLibraryOptions :: LibraryOptions
defLibraryOptions = LibraryOptions
    { library = ShellLibrary Bash
    , output = OutputStdoutOnly
    , importOrContent = Content
    }

-- | Defines which library content or import will be produced.
data Library
    = ShellLibrary Shell
    | DhallLibrary DhallLibrary
  deriving stock (Generic, Show)

instance HasShell Library where
    updateShell f = Endo \case
        ShellLibrary shell ->
            ShellLibrary (f `appEndo` shell)
        DhallLibrary _ ->
            -- Bash is the default shell.
            ShellLibrary (f `appEndo` Bash)

data ImportOrContent
    = Import
    -- ^ Produce import statement for the library.
    | Content
    -- ^ Produce library content.
  deriving stock (Generic, Show)

putLibrary
    :: (forall ann. Pretty.Doc ann)
    -> Config
    -> LibraryOptions
    -> IO ()
putLibrary subcmd config LibraryOptions{..} = case library of
    ShellLibrary shell ->
        case shell of
            Bash ->
                putBashLibrary subcmd config importOrContent output
            _ ->
                dieUnsupportedShell subcmd config shell

    DhallLibrary dhallLib ->
        putDhallLibrary config dhallLib importOrContent output

dieUnsupportedShell
    :: (forall ann. Pretty.Doc ann)
    -> Config
    -> Shell
    -> IO a
dieUnsupportedShell subcmd Config{colourOutput, verbosity} shell = do
    errorMsg subcmd verbosity colourOutput stderr
        ( fromString (show shell)
        <> ": Unsupported SHELL value."
        )
    exitWith (ExitFailure 125)

-- {{{ Script and Dhall Libraries -- Bash Library -----------------------------

putBashLibrary
    :: (forall ann. Pretty.Doc ann)
    -> Config
    -> ImportOrContent
    -> OutputStdoutOrFile
    -> IO ()
putBashLibrary subcmd config importOrContent = \case
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
            Left e ->
                -- TODO: Error handling  should probably be left to higher
                -- level code.
                dieFailedToGenerateLibrary subcmd config e

            Right t ->
                pure t

dieFailedToGenerateLibrary
    :: (forall ann. Pretty.Doc ann)
    -> Config
    -> Dhall.ExtractErrors Dhall.Src Void
    -> IO a
dieFailedToGenerateLibrary subcmd Config{colourOutput, verbosity} err = do
    errorMsg subcmd verbosity colourOutput stderr
        ( "Failed to generate library or its import with: "
        <> fromString (show err)
        )
    -- TODO: This is probably not the best exit code.
    exitWith (ExitFailure 1)

-- }}} Script and Dhall Libraries -- Bash Library -----------------------------

-- {{{ Script and Dhall Libraries -- Dhall Libraries --------------------------

data DhallLibrary
    = LatestPrelude
    | PreludeV14_0_0
    | PreludeV13_0_0
    | PreludeV12_0_0
    | PreludeV11_1_0
    | CommandWrapper
    | CommandWrapperExec
  deriving stock (Bounded, Enum, Generic, Show)

parseDhallLibrary :: (Eq s, IsString s) => s -> Maybe DhallLibrary
parseDhallLibrary = \case
    "prelude"         -> Just LatestPrelude
    "prelude-v11.1.0" -> Just PreludeV11_1_0
    "prelude-v12.0.0" -> Just PreludeV12_0_0
    "prelude-v13.0.0" -> Just PreludeV13_0_0
    "prelude-v14.0.0" -> Just PreludeV14_0_0
    "command-wrapper" -> Just CommandWrapper
    "exec"            -> Just CommandWrapperExec
    _                 -> Nothing

showDhallLibrary :: IsString s => DhallLibrary -> s
showDhallLibrary = \case
    LatestPrelude      -> "prelude"
    PreludeV11_1_0     -> "prelude-v11.1.0"
    PreludeV12_0_0     -> "prelude-v12.0.0"
    PreludeV13_0_0     -> "prelude-v13.0.0"
    PreludeV14_0_0     -> "prelude-v14.0.0"
    CommandWrapper     -> "command-wrapper"
    CommandWrapperExec -> "exec"

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
        (PreludeV11_1_0, Content) ->
            hPutExpr h $(Dhall.TH.staticDhallExpression preludeV11_1_0Content)

        (PreludeV11_1_0, Import) ->
            Text.hPutStrLn h preludeV11_1_0Import

        (PreludeV12_0_0, Content) ->
            hPutExpr h $(Dhall.TH.staticDhallExpression preludeV12_0_0Content)

        (PreludeV12_0_0, Import) ->
            Text.hPutStrLn h preludeV12_0_0Import

        (PreludeV13_0_0, Content) ->
            hPutExpr h $(Dhall.TH.staticDhallExpression preludeV13_0_0Content)

        (PreludeV13_0_0, Import) ->
            Text.hPutStrLn h preludeV13_0_0Import

        (PreludeV14_0_0, Content) ->
            hPutExpr h $(Dhall.TH.staticDhallExpression preludeV14_0_0Content)

        (PreludeV14_0_0, Import) ->
            Text.hPutStrLn h preludeV14_0_0Import

        (LatestPrelude, Content) ->
            hPutExpr h $(Dhall.TH.staticDhallExpression preludeV14_0_0Content)

        (LatestPrelude, Import) ->
            Text.hPutStrLn h preludeV14_0_0Import

        -- IMPORTANT!
        --
        -- Whenever one of these fails to compile then Command
        -- Wrapper library was updated, and both hash and import
        -- need updating.  Unfortunately it won't always fail due
        -- to caching. Keeping this up to date will be a pain.
        --
        -- TODO: Figure out how to do this in a better way.

        (CommandWrapper, Content) ->
            hPutExpr h $(Dhall.TH.staticDhallExpression commandWrapperContent)

        (CommandWrapper, Import) ->
            Text.hPutStrLn h commandWrapperImport

        (CommandWrapperExec, Content) ->
            hPutExpr h $(Dhall.TH.staticDhallExpression execContent)

        (CommandWrapperExec, Import) ->
            Text.hPutStrLn h execImport

-- }}} Script and Dhall Libraries -- Dhall Libraries --------------------------

-- }}} Script and Dhall Libraries ---------------------------------------------

-- {{{ Shell Completion Script ------------------------------------------------

data ScriptOptions = ScriptOptions
    { aliases :: [String]
    , shell :: Shell
    , subcommand :: Maybe String
    , output :: OutputStdoutOrFile
    }
  deriving stock (Generic, Show)
  deriving anyclass (HasShell)

instance HasOutput ScriptOptions where
    type Output ScriptOptions = OutputStdoutOrFile
    output = typed

defScriptOptions :: ScriptOptions
defScriptOptions = ScriptOptions
    { shell = Bash
    , subcommand = Nothing
    , aliases = []
    , output = OutputStdoutOnly
    }

type MkCompletionScript =
           Shell
        -> Text
        -> Text
        -> Maybe Text
        -> [Text]
        -> Text

putShellCompletionScript
    :: (forall ann. Pretty.Doc ann)
    -> AppNames
    -> Config
    -> ScriptOptions
    -> IO ()
putShellCompletionScript subcmd appNames config ScriptOptions{..} =
    case validationToEither script of
        Left e ->
            dieFailedToGenerateCompletionScript subcmd config e

        Right (mkScript :: MkCompletionScript) -> do
            let completionScript = mkScript
                    shell
                    (fromString (usedName appNames) :: Text)
                    (fromString (exePath appNames) :: Text)
                    (fromString <$> subcommand :: Maybe Text)
                    (fromString <$> aliases :: [Text])

            case output of
                OutputHandle _ ->
                    Text.putStr completionScript

                OutputNotHandle (OutputFile fn) ->
                    Text.atomicWriteFile fn completionScript
  where
    script = Dhall.extract Dhall.auto
        $(Dhall.TH.staticDhallExpression shellCompletionTemplate)

dieFailedToGenerateCompletionScript
    :: (forall ann. Pretty.Doc ann)
    -> Config
    -> Dhall.ExtractErrors Dhall.Src Void
    -> IO a
dieFailedToGenerateCompletionScript
  subcmd Config{colourOutput, verbosity} err = do
    errorMsg subcmd verbosity colourOutput stderr
        ( "Failed to generate completion script: "
        <> fromString (show err)
        )
    -- TODO: This is probably not the best exit code.
    exitWith (ExitFailure 1)

-- }}} Shell Completion Script ------------------------------------------------