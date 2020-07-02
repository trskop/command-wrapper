{-# LANGUAGE TemplateHaskell #-}
-- |
-- Module:      $Header$
-- Description: Print requested library to stdout or write it into a file.
-- Copyright:   (c) 2019-2020 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions; POSIX.
--
-- Print requested library to stdout or write it into a file.
module CommandWrapper.Toolset.InternalSubcommand.Completion.Libraries
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

    -- ** Direnv Library
    , putDirenvLibrary

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
import Data.Maybe (Maybe(Just, Nothing), fromMaybe)
import Data.Monoid (Endo(Endo, appEndo), (<>))
import Data.String (IsString, String, fromString)
import Data.Void (Void)
import GHC.Generics (Generic)
import System.Exit (ExitCode(ExitFailure), exitWith)
import System.IO (FilePath, IO, IOMode(WriteMode), stderr, stdout, withFile)
import Text.Show (Show, show)

import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString (putStr)
--import Data.ByteString.ShellEscape (bash, bytes)
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

import CommandWrapper.Core.Config.Shell
    ( HasShell(..)
    , Shell(..)
    )
import CommandWrapper.Core.Environment (AppNames(exePath, usedName))
import CommandWrapper.Core.Message (errorMsg)
import CommandWrapper.Toolset.Config.Global
    ( Config(Config, colourOutput, verbosity)
    )
import qualified CommandWrapper.Toolset.InternalSubcommand.Config.Dhall as Dhall
    ( hPutExpr
    )
import CommandWrapper.Toolset.InternalSubcommand.Completion.DhallExpressions
    ( commandWrapperContent
    , commandWrapperImport
    , execContent
    , execImport
    , preludeV17_0_0Content
    , preludeV17_0_0Import
    , shellCompletionTemplate
    )


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
    | DirenvLibrary
  deriving stock (Generic, Show)

instance HasShell Library where
    updateShell f = Endo \case
        ShellLibrary shell ->
            ShellLibrary (f `appEndo` shell)
        DhallLibrary _ ->
            ShellLibrary (f `appEndo` defaultShell)
        DirenvLibrary ->
            ShellLibrary (f `appEndo` defaultShell)
      where
        defaultShell = Bash

data ImportOrContent
    = Import
    -- ^ Produce import statement for the library.
    | Content
    -- ^ Produce library content.
  deriving stock (Generic, Show)

putLibrary
    :: (forall ann. Pretty.Doc ann)
    -> AppNames
    -> Config
    -> LibraryOptions
    -> IO ()
putLibrary subcmd appNames config LibraryOptions{..} = case library of
    ShellLibrary shell ->
        case shell of
            Bash ->
                putBashLibrary subcmd config importOrContent output
            _ ->
                dieUnsupportedShell subcmd config shell

    DhallLibrary dhallLib ->
        putDhallLibrary config dhallLib importOrContent output

    DirenvLibrary ->
        putDirenvLibrary subcmd appNames config importOrContent output

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
    libContent = $(embedFile "./bash/lib.bash")

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

-- }}} Script and Dhall Libraries -- Bash Library -----------------------------

-- {{{ Script and Dhall Libraries -- Direnv Library ---------------------------

putDirenvLibrary
    :: (forall ann. Pretty.Doc ann)
    -> AppNames
    -> Config
    -> ImportOrContent
    -> OutputStdoutOrFile
    -> IO ()
putDirenvLibrary subcmd appNames config importOrContent = \case
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
    libContent = $(embedFile "./bash/direnv.bash")

    libImportScriptExpr =
        $(Dhall.TH.staticDhallExpression "./dhall/import-direnv-library.dhall")

    getLibImportScript :: IO Text
    getLibImportScript = do
        let script = Dhall.extract Dhall.auto libImportScriptExpr
        case validationToEither script of
            Left e ->
                -- TODO: Error handling  should probably be left to higher
                -- level code.
                dieFailedToGenerateLibrary subcmd config e

            Right f ->
                pure (f (usedName appNames))

-- }}} Script and Dhall Libraries -- Direnv Library ---------------------------

-- {{{ Script and Dhall Libraries -- Dhall Libraries --------------------------

data DhallLibrary
    = LatestPrelude
    | PreludeV17_0_0
    | CommandWrapper
    | CommandWrapperExec
  deriving stock (Bounded, Enum, Generic, Show)

parseDhallLibrary :: (Eq s, IsString s) => s -> Maybe DhallLibrary
parseDhallLibrary = \case
    "prelude"         -> Just LatestPrelude
    "prelude-v17.0.0" -> Just PreludeV17_0_0
    "command-wrapper" -> Just CommandWrapper
    "exec"            -> Just CommandWrapperExec
    _                 -> Nothing

showDhallLibrary :: IsString s => DhallLibrary -> s
showDhallLibrary = \case
    LatestPrelude      -> "prelude"
    PreludeV17_0_0     -> "prelude-v17.0.0"
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
        (PreludeV17_0_0, Content) ->
            hPutExpr h $(Dhall.TH.staticDhallExpression preludeV17_0_0Content)

        (PreludeV17_0_0, Import) ->
            Text.hPutStrLn h preludeV17_0_0Import

        (LatestPrelude, Content) ->
            hPutExpr h $(Dhall.TH.staticDhallExpression preludeV17_0_0Content)

        (LatestPrelude, Import) ->
            Text.hPutStrLn h preludeV17_0_0Import

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
    , overrideExecutablePath :: Maybe FilePath
    , overrideToolsetName :: Maybe String
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
    , overrideExecutablePath = Nothing
    , overrideToolsetName = Nothing
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
                    (fromString toolsetName :: Text)
                    (fromString exePath' :: Text)
                    (fromString <$> subcommand :: Maybe Text)
                    (fromString <$> aliases :: [Text])

            case output of
                OutputHandle _ ->
                    Text.putStr completionScript

                OutputNotHandle (OutputFile fn) ->
                    Text.atomicWriteFile fn completionScript
  where
    exePath' :: FilePath
    exePath' = fromMaybe (exePath appNames) overrideExecutablePath

    toolsetName :: String
    toolsetName = fromMaybe (usedName appNames) overrideToolsetName

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

-- }}} Shell Completion Script ------------------------------------------------
