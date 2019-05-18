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
    ( -- * Options
      Options(..)
    , Mode(..)

      -- * Execution
    , command

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
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Text.Prettyprint.Doc (Doc, Pretty)
import Dhall.Binary (defaultStandardVersion)
import Dhall.Core (Expr(..), Import)
import Dhall.Import (Imported(..))
import Dhall.Parser (Src)
import Dhall.Pretty (Ann, CharacterSet(..), annToAnsiStyle, layoutOpts)
import Dhall.TypeCheck (DetailedTypeError(..), TypeError, X)
import System.Exit (exitFailure)
import System.IO (Handle)

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

import Data.Generics.Internal.VL.Lens ((^.))


-- | Top-level program options
data Options = Options
    { mode            :: Mode
    , explain         :: Bool
    , plain           :: Bool
    , inputEncoding   :: InputEncoding
    , outputEncoding  :: OutputEncoding

    , input           :: ()
    , output          :: ()
    }
  deriving (Show)

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
    = Default { file :: Maybe FilePath, annotate :: Bool, alpha :: Bool }
    | Resolve { file :: Maybe FilePath, resolveMode :: Maybe ResolveMode }
    | Type { file :: Maybe FilePath }
    | Normalize { file :: Maybe FilePath, alpha :: Bool }
    | Format { formatMode :: Dhall.Format.FormatMode }
    | Freeze { inplace :: Maybe FilePath, all_ :: Bool }
    | Hash
    | Lint { inplace :: Maybe FilePath }
    | Encode { file :: Maybe FilePath, json :: Bool }
    | Decode { file :: Maybe FilePath, json :: Bool }
  deriving (Show)

deriving instance Show Dhall.Format.FormatMode  -- TODO: Get rid of orphan

data Diff = Diff
    { expr1 :: Text
    , expr2 :: Text
--  , output :: Maybe FilePath  -- TODO Ist this a correct type?
    , plain :: Bool  -- TODO This should be handled by ColourOutput.
    }
  deriving (Show)

defDiff :: Text -> Text -> Diff
defDiff expr1 expr2 = Diff{expr1, expr2, plain = False}

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

data ResolveMode
    = Dot
    | ListTransitiveDependencies
    | ListImmediateDependencies
  deriving (Show)

getExpression :: Maybe FilePath -> IO (Expr Src Import)
getExpression maybeFile = do
    inText <- do
        case maybeFile of
            Just "-"  -> Data.Text.IO.getContents
            Just file -> Data.Text.IO.readFile file
            Nothing   -> Data.Text.IO.getContents

    Dhall.Core.throws (Dhall.Parser.exprFromText "(stdin)" inText)

-- TODO: In principle this (command) should be implemented as:
--
-- ```
-- readInput options >>= runMode mode >>= writeOutput options
-- ```
--
-- Main idea is that input and output reading/writing and encoding should be
-- orthogonal to the mode that we are running in.

-- | Run the command specified by the `Options` type
command :: Options -> IO ()
command Options{..} = do
    -- TODO: Handle unicode/ascii correctly.
    let characterSet = Unicode -- case ascii of
--          True  -> ASCII
--          False -> Unicode

    GHC.IO.Encoding.setLocaleEncoding System.IO.utf8

    let toStatus maybeFile = Dhall.Import.emptyStatus file
          where
            file = case maybeFile of
                Just "-" -> "."
                Just f   -> f
                Nothing  -> "."

    let handle =
                Control.Exception.handle handler2
            .   Control.Exception.handle handler1
            .   Control.Exception.handle handler0
          where
            handler0 e = do
                let _ = e :: TypeError Src X
                System.IO.hPutStrLn System.IO.stderr ""
                if explain
                    then Control.Exception.throwIO (DetailedTypeError e)
                    else do
                        Data.Text.IO.hPutStrLn System.IO.stderr "\ESC[2mUse \"dhall --explain\" for detailed errors\ESC[0m"
                        Control.Exception.throwIO e

            handler1 (Imported ps e) = do
                let _ = e :: TypeError Src X
                System.IO.hPutStrLn System.IO.stderr ""
                if explain
                    then Control.Exception.throwIO (Imported ps (DetailedTypeError e))
                    else do
                        Data.Text.IO.hPutStrLn System.IO.stderr "\ESC[2mUse \"dhall --explain\" for detailed errors\ESC[0m"
                        Control.Exception.throwIO (Imported ps e)

            handler2 e = do
                let string = show (e :: SomeException)

                if not (null string)
                    then System.IO.hPutStrLn System.IO.stderr string
                    else return ()

                System.Exit.exitFailure

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
        render h expression = do
            let doc = Dhall.Pretty.prettyCharacterSet characterSet expression

            renderDoc h doc

    handle $ case mode of
        Default {..} -> do
            expression <- getExpression file

            resolvedExpression <- State.evalStateT (Dhall.Import.loadWith expression) (toStatus file)

            inferredType <- Dhall.Core.throws (Dhall.TypeCheck.typeOf resolvedExpression)

            let normalizedExpression = Dhall.Core.normalize resolvedExpression

            let alphaNormalizedExpression =
                    if alpha
                    then Dhall.Core.alphaNormalize normalizedExpression
                    else normalizedExpression

            let annotatedExpression =
                    if annotate
                        then Annot alphaNormalizedExpression inferredType
                        else alphaNormalizedExpression

            render System.IO.stdout annotatedExpression

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
            expression <- getExpression file

            resolvedExpression <- Dhall.Import.assertNoImports expression

            _ <- Dhall.Core.throws (Dhall.TypeCheck.typeOf resolvedExpression)

            let normalizedExpression = Dhall.Core.normalize resolvedExpression

            let alphaNormalizedExpression =
                    if alpha
                    then Dhall.Core.alphaNormalize normalizedExpression
                    else normalizedExpression

            render System.IO.stdout alphaNormalizedExpression

        Type {..} -> do
            expression <- getExpression file

            resolvedExpression <- Dhall.Import.assertNoImports expression

            inferredType <- Dhall.Core.throws (Dhall.TypeCheck.typeOf resolvedExpression)

            render System.IO.stdout (Dhall.Core.normalize inferredType)

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

diff :: Diff -> IO ()
diff Diff{..} = do
    diffDoc <- Dhall.Diff.diffNormalized
        <$> Dhall.inputExpr expr1
        <*> Dhall.inputExpr expr2

    renderDoc System.IO.stdout diffDoc
  where
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

repl :: Repl -> IO ()
repl Repl{..} =
    Dhall.Repl.repl characterSet explain defaultStandardVersion
