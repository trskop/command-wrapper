{-# LANGUAGE StandaloneDeriving #-}
-- |
-- Module:      $Header$
-- Description: Modified version of Dhall.Main from dhall-haskell package.
-- Copyright:   (c) 2018-2019 Gabriel Gonzalez and contrubutors;
--              (c) 2019 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Modified version of @Dhall.Main@ from @dhall-haskell@ package.
module CommandWrapper.Internal.Subcommand.Config.Dhall
    (
    -- * Interpreter
      Options(..)
    , Mode(..)
    , defOptions
    , interpreter

    -- * Diff
    , Diff(..)
    , defDiff
    , diff

    -- * REPL
    , Repl(..)
    , defRepl
    , repl
    )
  where

import Control.Exception (SomeException)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import System.Exit (exitFailure)
import System.IO (Handle)

import Data.Text (Text)
import Data.Text.Prettyprint.Doc (Doc, Pretty)
import Dhall.Binary (defaultStandardVersion)
import Dhall.Core (Expr(..), Import)
import Dhall.Import (Imported(..))
import Dhall.Parser (Src)
import Dhall.Pretty (Ann, CharacterSet(..), annToAnsiStyle, layoutOpts)
import Dhall.TypeCheck (DetailedTypeError(..), TypeError, X)

import qualified Codec.CBOR.JSON
import qualified Codec.CBOR.Read
import qualified Codec.CBOR.Write
import qualified Codec.Serialise
import qualified Control.Exception
import qualified Control.Monad.Trans.State.Strict          as State
import qualified Data.Aeson
import qualified Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy
import qualified Data.ByteString.Lazy.Char8
--import qualified Data.Text
import qualified Data.Text.IO
import qualified Data.Text.Prettyprint.Doc                 as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Pretty
import qualified Dhall
import qualified Dhall.Binary
import qualified Dhall.Core
import qualified Dhall.Diff
import qualified Dhall.Format
import qualified Dhall.Freeze
import qualified Dhall.Hash
import qualified Dhall.Import
--import qualified Dhall.Import.Types
import qualified Dhall.Lint
import qualified Dhall.Parser
import qualified Dhall.Pretty
import qualified Dhall.Repl
import qualified Dhall.TypeCheck
import qualified GHC.IO.Encoding
import qualified System.Console.ANSI
import qualified System.IO
--import qualified Text.Dot
import qualified Data.Map

import CommandWrapper.Config.Global (Config(Config, colourOutput, verbosity))
import CommandWrapper.Environment (AppNames(AppNames, usedName))
import qualified CommandWrapper.Internal.Dhall as Dhall (hPutExpr)
import CommandWrapper.Options.ColourOutput (ColourOutput)
import qualified CommandWrapper.Options.ColourOutput as ColourOutput (ColourOutput(Auto))
import Data.Generics.Internal.VL.Lens ((^.))
import Data.Verbosity (Verbosity)
import qualified Data.Verbosity as Verbosity (Verbosity(Normal))


-- | Top-level program options
data Options = Options
    { mode            :: Mode
--  , inputEncoding   :: InputEncoding
--  , outputEncoding  :: OutputEncoding

    , input           :: Input
    , output          :: Output
    }
  deriving (Show)

defOptions :: Options
defOptions = Options
    { mode = Default
        { annotate = False
        , alpha = False
        , allowImports = True
        , showType = False
        }
--  , inputEncoding   =
--  , outputEncoding  =
    , input = InputStdin
    , output = ()
    }

data Input
    = InputStdin
    | InputFile FilePath
  deriving (Show)

type Output = () -- TODO Implement

-- TODO: Alternative to simple (input, output) product we should consider:
--
-- data InputOutput
--    = InputOutput Input Output
--    | Inplace Input
--
-- Alternative representation is to have
--
-- data Output
--    = Output ...
--    | Inplace  -- Reuse 'Input' as output with few gotchas as temp files etc.

data InputEncoding
    = InputCbor
    | InputCborJson
    | InputDhall
    | InputJson
    | InputText
    | InputYaml
  deriving (Show)

data OutputEncoding
    = OutputCbor
    | OutputCborJson
    | OutputDhallAscii
    | OutputDhallUnicode
    | OutputJson
    | OutputYaml
  deriving (Show)

-- | The subcommands for the @dhall@ executable
data Mode
    = Default
        { allowImports :: Bool
        , alpha :: Bool
        , annotate :: Bool
        , showType :: Bool
        }
    | Resolve { resolveMode :: Maybe ResolveMode }
--  | Format { formatMode :: Dhall.Format.FormatMode }
--  | Freeze { inplace :: Maybe FilePath, all_ :: Bool }
--  | Hash
--  | Lint { inplace :: Maybe FilePath }
--  | Encode { json :: Bool }
--  | Decode { json :: Bool }
  deriving (Show)

deriving instance Show Dhall.Format.FormatMode  -- TODO: Get rid of orphan

data ResolveMode
    = Dot
    | ListTransitiveDependencies
    | ListImmediateDependencies
  deriving (Show)

readExpression :: Input -> IO (Expr Src Import, Dhall.Import.Status IO)
readExpression = \case
    InputStdin ->
        Data.Text.IO.getContents >>= parseExpr "(stdin)" "."

    InputFile file ->
        Data.Text.IO.readFile file >>= parseExpr file file
  where
    parseExpr f c txt =
        (,) <$> Dhall.Core.throws (Dhall.Parser.exprFromText f txt)
            <*> pure (Dhall.Import.emptyStatus c)

-- TODO: In principle this should be implemented as:
--
-- ```
-- readExpression options >>= runMode mode >>= writeExpression options
-- ```
--
-- Main idea is that input and output reading/writing and encoding should be
-- orthogonal to the mode that we are running in.

interpreter :: AppNames -> Config -> Options -> IO ()
interpreter
  AppNames{usedName}
  Config{colourOutput, verbosity}
  Options{input, mode} = do
    GHC.IO.Encoding.setLocaleEncoding System.IO.utf8

    (expression, status) <- readExpression input

    handle case mode of
        Default{allowImports, alpha, annotate, showType} -> do
            (resolvedExpression, inferredType) <- do
                expr <- if allowImports
                    then
                        State.evalStateT (Dhall.Import.loadWith expression)
                            status
                    else
                        Dhall.Import.assertNoImports expression

                exprType <- Dhall.Core.throws (Dhall.TypeCheck.typeOf expr)

                if showType
                    then
                        (exprType,) <$> Dhall.Core.throws
                            (Dhall.TypeCheck.typeOf exprType)
                    else
                        pure (expr, exprType)

            let normalizedExpression = Dhall.Core.normalize resolvedExpression

                alphaNormalizedExpression =
                    if alpha
                        then Dhall.Core.alphaNormalize normalizedExpression
                        else normalizedExpression

                annotatedExpression =
                    if annotate
                        then Annot alphaNormalizedExpression inferredType
                        else alphaNormalizedExpression

            putExpr annotatedExpression

        Resolve{resolveMode} -> case resolveMode of
            Just Dot -> pure () -- TODO: Die with proper error message.

            Just ListImmediateDependencies ->
                mapM_ (print . Pretty.pretty . Dhall.Core.importHashed)
                    expression

            Just ListTransitiveDependencies -> do
                cache <- (^. Dhall.Import.cache)
                    <$> State.execStateT (Dhall.Import.loadWith expression)
                        status

                mapM_ (print . Pretty.pretty . Dhall.Core.importType . Dhall.Core.importHashed)
                     (Data.Map.keys cache)

            Nothing -> do
                (resolvedExpression, _) <- State.runStateT
                    (Dhall.Import.loadWith expression) status

                putExpr resolvedExpression
  where
    characterSet = Unicode -- TODO: This should be configurable.

    putExpr = Dhall.hPutExpr (fromMaybe ColourOutput.Auto colourOutput)
        characterSet System.IO.stdout

    handle =
        Control.Exception.handle handler2
        . Control.Exception.handle handler1
        . Control.Exception.handle handler0

    explain = verbosity > Verbosity.Normal

    handler0 :: TypeError Src X -> IO ()
    handler0 e = do
        System.IO.hPutStrLn System.IO.stderr ""
        if explain
            then Control.Exception.throwIO (DetailedTypeError e)
            else do
                -- TODO: Wrong message.
                Data.Text.IO.hPutStrLn System.IO.stderr "\ESC[2mUse \"dhall --explain\" for detailed errors\ESC[0m"
                Control.Exception.throwIO e

    handler1 :: Imported (TypeError Src X) -> IO ()
    handler1 (Imported ps e) = do
        System.IO.hPutStrLn System.IO.stderr ""
        if explain
            then Control.Exception.throwIO (Imported ps (DetailedTypeError e))
            else do
                -- TODO: Wrong message.
                Data.Text.IO.hPutStrLn System.IO.stderr "\ESC[2mUse \"dhall --explain\" for detailed errors\ESC[0m"
                Control.Exception.throwIO (Imported ps e)

    handler2 :: SomeException -> IO ()
    handler2 e = do
        let string = show e

        if not (null string)
            -- TODO: Use errorMsg
            then System.IO.hPutStrLn System.IO.stderr string
            else return ()

        System.Exit.exitFailure

{- TODO: Reimplement the rest of Dhall commands/actions

-- | Run the command specified by the `Options` type
command :: Options -> IO ()
command Options{..} = do
    -- ...
    let renderDoc :: Handle -> Doc Ann -> IO ()
        renderDoc h doc = do
            let stream = Pretty.layoutSmart layoutOpts doc

            supportsANSI <- System.Console.ANSI.hSupportsANSI h
            let ansiStream =
                    if supportsANSI && not plain
                    then fmap annToAnsiStyle stream
                    else Pretty.unAnnotateS stream

            Pretty.renderIO h ansiStream
            Data.Text.IO.hPutStrLn h ""

    let render :: Pretty a => Handle -> Expr s a -> IO ()
        render = -- ...

    handle $ case mode of
        Default {..} -> do
            -- Not relevant any more.

        -- TODO: _dot field of Status is not available.
        Resolve { resolveMode = Just Dot, ..} -> pure () -- do
--          expression <- getExpression file

--          (Dhall.Import.Types.Status { _dot}) <-
--              State.execStateT (Dhall.Import.loadWith expression) (toStatus file)

--          putStr . ("strict " <>) . Text.Dot.showDot $
--                 Text.Dot.attribute ("rankdir", "LR") >>
--                 _dot

        Resolve { resolveMode = Just ListImmediateDependencies, ..} -> do
            expression <- getExpression file

            mapM_ (print
                        . Pretty.pretty
                        . Dhall.Core.importHashed) expression

        Resolve { resolveMode = Just ListTransitiveDependencies, ..} -> do
            expression <- getExpression file

            status <- State.execStateT (Dhall.Import.loadWith expression) (toStatus file)

            mapM_ print
                 .   fmap (   Pretty.pretty
                          .   Dhall.Core.importType
                          .   Dhall.Core.importHashed )
                 .   Data.Map.keys
                 $   status ^. Dhall.Import.cache

        Resolve { resolveMode = Nothing, ..} -> do
            expression <- getExpression file

            (resolvedExpression, _) <-
                State.runStateT (Dhall.Import.loadWith expression) (toStatus file)
            render System.IO.stdout resolvedExpression

        Normalize {..} -> do
            -- Not relevant any more.

        Type {..} -> do
            -- Not relevant any more.

        Format {..} -> do
            Dhall.Format.format (Dhall.Format.Format {..})

        Freeze {..} -> do
            Dhall.Freeze.freeze inplace all_ characterSet defaultStandardVersion

        Hash -> do
            Dhall.Hash.hash defaultStandardVersion

        Lint {..} -> do
            case inplace of
                Just file -> do
                    text <- Data.Text.IO.readFile file

                    (header, expression) <- Dhall.Core.throws (Dhall.Parser.exprAndHeaderFromText file text)

                    let lintedExpression = Dhall.Lint.lint expression

                    let doc =   Pretty.pretty header
                            <>  Dhall.Pretty.prettyCharacterSet characterSet lintedExpression

                    System.IO.withFile file System.IO.WriteMode (\h -> do
                        renderDoc h doc )

                Nothing -> do
                    text <- Data.Text.IO.getContents

                    (header, expression) <- Dhall.Core.throws (Dhall.Parser.exprAndHeaderFromText "(stdin)" text)

                    let lintedExpression = Dhall.Lint.lint expression

                    let doc =   Pretty.pretty header
                            <>  Dhall.Pretty.prettyCharacterSet characterSet lintedExpression

                    renderDoc System.IO.stdout doc

        Encode {..} -> do
            expression <- getExpression file

            let term = Dhall.Binary.encode expression

            let bytes = Codec.Serialise.serialise term

            if json
                then do
                    let decoder = Codec.CBOR.JSON.decodeValue False

                    (_, value) <- Dhall.Core.throws (Codec.CBOR.Read.deserialiseFromBytes decoder bytes)

                    let jsonBytes = Data.Aeson.Encode.Pretty.encodePretty value

                    Data.ByteString.Lazy.Char8.putStrLn jsonBytes

                else do
                    Data.ByteString.Lazy.putStr bytes

        Decode {..} -> do
            bytes <- do
                case file of
                    Just f  -> Data.ByteString.Lazy.readFile f
                    Nothing -> Data.ByteString.Lazy.getContents

            term <- do
                if json
                    then do
                        value <- case Data.Aeson.eitherDecode' bytes of
                            Left  string -> fail string
                            Right value  -> return value

                        let encoding = Codec.CBOR.JSON.encodeValue value

                        let cborBytes = Codec.CBOR.Write.toLazyByteString encoding
                        Dhall.Core.throws (Codec.Serialise.deserialiseOrFail cborBytes)
                    else do
                        Dhall.Core.throws (Codec.Serialise.deserialiseOrFail bytes)

            expression <- Dhall.Core.throws (Dhall.Binary.decodeExpression term)

            let doc = Dhall.Pretty.prettyCharacterSet characterSet expression

            renderDoc System.IO.stdout doc
-}

-- {{{ Diff -------------------------------------------------------------------

data Diff = Diff
    { expr1 :: Text
    , expr2 :: Text
--  , output :: Maybe FilePath  -- TODO Ist this a correct type?
    , plain :: Bool  -- TODO This should be handled by ColourOutput.
    }
  deriving (Show)

defDiff :: Text -> Text -> Diff
defDiff expr1 expr2 = Diff{expr1, expr2, plain = False}

diff :: Diff -> IO ()
diff Diff{..} = do
    diffDoc <- Dhall.Diff.diffNormalized
        <$> Dhall.inputExpr expr1
        <*> Dhall.inputExpr expr2

    renderDoc System.IO.stdout diffDoc
  where
    -- TODO: Merge with other functions that we have for this purpose?
    renderDoc :: Handle -> Doc Ann -> IO ()
    renderDoc h doc = do
        let stream = Pretty.layoutSmart layoutOpts doc

        supportsANSI <- System.Console.ANSI.hSupportsANSI h
        let ansiStream =
                if supportsANSI && not plain
                then fmap annToAnsiStyle stream
                else Pretty.unAnnotateS stream

        Pretty.renderIO h ansiStream
        Data.Text.IO.hPutStrLn h ""

-- }}} Diff -------------------------------------------------------------------

-- {{{ REPL -------------------------------------------------------------------

data Repl = Repl
    { characterSet :: CharacterSet
    , explain :: Bool

    , historyFile :: Maybe FilePath -- TODO Is this a correct type?
    -- ^ This is not currently supported by neither @dhall@ library nor by
    -- @repline@ which is used by @dhall@ to implement REPL functionality.
    }
  deriving (Show)

deriving instance Show CharacterSet -- TODO Get rid of orphan

defRepl :: Repl
defRepl = Repl
    { characterSet = Unicode
    , explain = False
    , historyFile = Nothing
    }

repl :: Repl -> IO ()
repl Repl{..} =
    Dhall.Repl.repl characterSet explain defaultStandardVersion

-- }}} REPL -------------------------------------------------------------------
