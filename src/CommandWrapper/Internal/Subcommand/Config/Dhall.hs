{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module:      $Header$
-- Description: Dhall operations provided by the config command.
-- Copyright:   (c) 2018-2019 Gabriel Gonzalez and contrubutors;
--              (c) 2019 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Dhall operations provided by the config command.  This module started its
-- life as a copy of @Dhall.Main@ from @dhall-haskell@ package, hence the
-- copyright.
module CommandWrapper.Internal.Subcommand.Config.Dhall
    (
    -- * Interpreter
      Interpreter(..)
    , defInterpreter
    , interpreter

    -- * Resolve
    , Resolve(..)
    , ResolveMode(..)
    , defResolve
    , resolve

    -- * Lint
    , Lint(..)
    , defLint
    , lint

    -- * Format
    , Dhall.Format.Format(..)
    , Dhall.Format.FormatMode(..)
    , defFormat
    , format

    -- * Freeze
    , Freeze(..)
    , defFreeze
    , freeze

    -- * Hash
    , hash

    -- * Diff
    , Diff(..)
    , defDiff
    , diff

    -- * REPL
    , Repl(..)
    , defRepl
    , repl

    -- * Input\/Output
    , Input(..)
    , Output(..)
    )
  where

import Control.Exception (Exception, SomeException, handle, throwIO)
import Control.Monad (unless)
import Data.Foldable (for_, traverse_)
import Data.List as List (dropWhile, span)
import Data.Monoid ((<>))
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import qualified GHC.IO.Encoding as IO (setLocaleEncoding)
import System.Exit (exitFailure)
import System.IO
    ( Handle
    , IOMode(WriteMode)
    , hPutStrLn
    , stderr
    , stdout
    , withFile
    )
import qualified System.IO as IO (utf8)

import Data.Generics.Product.Fields (field')
import Data.Output (IsOutput, HasOutput)
import qualified Data.Output (IsOutput(..), HasOutput(..))
import Data.Text (Text)
import qualified Data.Text.IO as Text (getContents, readFile)
import Data.Text.Prettyprint.Doc (Doc)
import qualified Data.Map as Map (keys)
import Dhall.Binary (defaultStandardVersion)
import Dhall.Core (Expr(..), Import)
import qualified Dhall.Freeze as Dhall (freezeImport, freezeRemoteImport)
import Dhall.Import (Imported(..), MissingImports)
import qualified Dhall.Lint as Dhall (lint)
import Dhall.Parser (ParseError, SourcedException, Src)
import qualified Dhall.Pretty as Dhall (Ann, CharacterSet(..))
import Dhall.TypeCheck (DetailedTypeError(..), TypeError, X)
import System.FilePath (takeDirectory)

--import qualified Codec.CBOR.JSON
--import qualified Codec.CBOR.Read
--import qualified Codec.CBOR.Write
--import qualified Codec.Serialise
import qualified Control.Monad.Trans.State.Strict as State
--import qualified Data.Aeson
--import qualified Data.Aeson.Encode.Pretty
--import qualified Data.ByteString.Lazy
--import qualified Data.ByteString.Lazy.Char8
--import qualified Data.Text
import qualified Data.Text.Prettyprint.Doc                 as Pretty
--import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Pretty
import qualified Dhall
--import qualified Dhall.Binary
import qualified Dhall.Core
import qualified Dhall.Diff
import qualified Dhall.Format
import qualified Dhall.Hash
import qualified Dhall.Import
--import qualified Dhall.Import.Types
import qualified Dhall.Parser
import qualified Dhall.Pretty
import qualified Dhall.Repl
import qualified Dhall.TypeCheck
--import qualified Text.Dot

import CommandWrapper.Config.Global (Config(Config, colourOutput, verbosity))
import CommandWrapper.Environment (AppNames(AppNames, usedName))
import qualified CommandWrapper.Internal.Dhall as Dhall (hPutDoc, hPutExpr)
import CommandWrapper.Options.ColourOutput (shouldUseColours)
import Data.Generics.Internal.VL.Lens ((^.))
import qualified Data.Verbosity as Verbosity (Verbosity(Normal, Silent))


-- {{{ Interpreter ------------------------------------------------------------

data Interpreter = Interpreter
    { allowImports :: Bool
    , alpha :: Bool
    , annotate :: Bool
    , showType :: Bool

--  , inputEncoding   :: InputEncoding
--  , outputEncoding  :: OutputEncoding

    , input           :: Input
    , output          :: Output
    }
  deriving (Generic, Show)

instance HasOutput Interpreter where
    type Output Interpreter = Output
    output = field' @"output"

defInterpreter :: Interpreter
defInterpreter = Interpreter
    { annotate = False
    , alpha = False
    , allowImports = True
    , showType = False

--  , inputEncoding   =
--  , outputEncoding  =
    , input = InputStdin
    , output = OutputStdout
    }

-- TODO: In principle this should be implemented as:
--
-- ```
-- readExpression options >>= runMode mode >>= writeExpression options
-- ```
--
-- Main idea is that input and output reading/writing and encoding should be
-- orthogonal to the mode that we are running in.

interpreter :: AppNames -> Config -> Interpreter -> IO ()
interpreter
  appNames
  config
  Interpreter{allowImports, alpha, annotate, input, showType}
  = handleExceptions appNames config do

    IO.setLocaleEncoding IO.utf8
    (expression, status) <- readExpression input

    (resolvedExpression, inferredType) <- do
        expr <- if allowImports
            then
                State.evalStateT (Dhall.Import.loadWith expression) status
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

    hPutExpr config stdout annotatedExpression

-- }}} Interpreter ------------------------------------------------------------

-- {{{ Resolve ----------------------------------------------------------------

data Resolve = Resolve
    { mode :: ResolveMode
    , input :: Input
    , output :: Output
    }
  deriving (Generic, Show)

instance HasOutput Resolve where
    type Output Resolve = Output
    output = field' @"output"

data ResolveMode
    = ResolveDependencies
    | ListTransitiveDependencies
    | ListImmediateDependencies
--  | Dot
  deriving (Show)

defResolve :: Resolve
defResolve = Resolve
    { mode = ResolveDependencies
    , input = InputStdin
    , output = OutputStdout
    }

resolve :: AppNames -> Config -> Resolve -> IO ()
resolve appNames config Resolve{mode, input, output} =
    handleExceptions appNames config do
        IO.setLocaleEncoding IO.utf8
        (expression, status) <- readExpression input

        case mode of
            ResolveDependencies -> do
                (resolvedExpression, _) <- State.runStateT
                    (Dhall.Import.loadWith expression) status

                withOutputHandle input output (hPutExpr config)
                    resolvedExpression

            ListImmediateDependencies ->
                -- TODO: Handle I/O correctly.
                traverse_ (print . Pretty.pretty . Dhall.Core.importHashed)
                    expression

            ListTransitiveDependencies -> do
                cache <- (^. Dhall.Import.cache)
                    <$> State.execStateT (Dhall.Import.loadWith expression)
                        status

                for_ (Map.keys cache)
                    -- TODO: Handle I/O correctly.
                    ( print
                    . Pretty.pretty
                    . Dhall.Core.importType
                    . Dhall.Core.importHashed
                    )

            -- TODO: Unable to implement at the moment.
--          Just Dot -> pure ()

-- }}} Resolve ----------------------------------------------------------------

-- {{{ Lint -------------------------------------------------------------------

data Lint = Lint
    { input :: Input
    , output :: Output
    , characterSet :: Dhall.CharacterSet
    }
  deriving (Generic, Show)

instance HasOutput Lint where
    type Output Lint = Output
    output = field' @"output"

defLint :: Lint
defLint = Lint
    { input = InputStdin
    , output = OutputStdout
    , characterSet = Dhall.Unicode
    }

lint :: AppNames -> Config -> Lint -> IO ()
lint appNames config Lint{input, output, characterSet} =
    handleExceptions appNames config do
        (header, expression) <- case input of
            InputStdin ->
                Text.getContents >>= parseExpr "(stdin)"

            InputFile file ->
                Text.readFile file >>= parseExpr file

        withOutputHandle input output (renderDoc config)
            (   Pretty.pretty header
            <>  Dhall.Pretty.prettyCharacterSet characterSet
                    (Dhall.lint expression)
            )
  where
    parseExpr f = Dhall.Core.throws . Dhall.Parser.exprAndHeaderFromText f

-- }}} Lint -------------------------------------------------------------------

{- TODO: Reimplement the rest of Dhall commands/actions

-- | Run the command specified by the `Options` type
command :: Options -> IO ()
command Options{..} = do
    handle $ case mode of
        -- ...
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

-- {{{ Format -----------------------------------------------------------------

deriving instance Show Dhall.Format.Format -- TODO Get rid of orphan
deriving instance Show Dhall.Format.FormatMode -- TODO Get rid of orphan

defFormat :: Dhall.Format.Format
defFormat = Dhall.Format.Format
    { characterSet = Dhall.Unicode
    , formatMode = Dhall.Format.Modify Nothing
    }

format :: AppNames -> Config -> Dhall.Format.Format -> IO ()
format appNames config = handleExceptions appNames config . Dhall.Format.format

-- }}} Format -----------------------------------------------------------------

-- {{{ Freeze -----------------------------------------------------------------

data Freeze = Freeze
    { remoteOnly :: Bool
    , input :: Input
    , output :: Output
    , characterSet :: Dhall.CharacterSet
    }
  deriving (Generic, Show)

instance HasOutput Freeze where
    type Output Freeze = Output
    output = field' @"output"

defFreeze :: Freeze
defFreeze = Freeze
    { remoteOnly = True
    , input = InputStdin
    , output = OutputStdout
    , characterSet = Dhall.Unicode
    }

freeze
    :: AppNames
    -> Config
    -> Freeze
    -> IO ()
freeze appNames config Freeze{..} = handleExceptions appNames config do
    (header, expression, directory) <- case input of
        InputStdin -> do
            (header, expression) <- Text.getContents
                >>= parseExpr "(stdin)"
            pure (header, expression, ".")

        InputFile file -> do
            (header, expression) <- Text.readFile file
                >>= parseExpr file
            pure (header, expression, takeDirectory file)

    let freezeFunction =
            ( if remoteOnly
                then Dhall.freezeRemoteImport
                else Dhall.freezeImport
            ) directory defaultStandardVersion

    frozenExpression <- traverse freezeFunction expression
    withOutputHandle input output (renderDoc config)
        ( Pretty.pretty header
        <> Dhall.Pretty.prettyCharacterSet characterSet frozenExpression
        )
  where
    parseExpr f = Dhall.Core.throws . Dhall.Parser.exprAndHeaderFromText f

-- }}} Freeze -----------------------------------------------------------------

-- {{{ Hash -------------------------------------------------------------------

hash :: AppNames -> Config -> IO ()
hash appNames config =
    handleExceptions appNames config (Dhall.Hash.hash defaultStandardVersion)

-- }}} Hash -------------------------------------------------------------------

-- {{{ Diff -------------------------------------------------------------------

data Diff = Diff
    { expr1 :: Text
    , expr2 :: Text
    , output :: Output
    }
  deriving (Generic, Show)

instance HasOutput Diff where
    type Output Diff = Output
    output = field' @"output"

defDiff :: Text -> Text -> Diff
defDiff expr1 expr2 = Diff
    { expr1
    , expr2
    , output = OutputStdout
    }

diff :: AppNames -> Config -> Diff -> IO ()
diff appNames config Diff{..} = handleExceptions appNames config do
    diffDoc <- Dhall.Diff.diffNormalized
        <$> Dhall.inputExpr expr1
        <*> Dhall.inputExpr expr2

    withOutputHandle InputStdin output (renderDoc config) diffDoc

-- }}} Diff -------------------------------------------------------------------

-- {{{ REPL -------------------------------------------------------------------

data Repl = Repl
    { characterSet :: Dhall.CharacterSet

    , historyFile :: Maybe FilePath -- TODO Is this a correct type?
    -- ^ This is not currently supported by neither @dhall@ library nor by
    -- @repline@ which is used by @dhall@ to implement REPL functionality.
    }
  deriving (Show)

deriving instance Show Dhall.CharacterSet -- TODO Get rid of orphan

defRepl :: Repl
defRepl = Repl
    { characterSet = Dhall.Unicode
    , historyFile = Nothing
    }

repl :: AppNames -> Config -> Repl -> IO ()
repl appNames config@Config{verbosity} Repl{..} =
    handleExceptions appNames config
        (Dhall.Repl.repl characterSet explain defaultStandardVersion)
  where
    explain = verbosity > Verbosity.Normal

-- }}} REPL -------------------------------------------------------------------

-- {{{ Input/Output -----------------------------------------------------------

data Input
    = InputStdin
    | InputFile FilePath
  deriving (Show)

data Output
    = OutputStdout
    | OutputBasedOnInput
    -- ^ If 'Input':
    --
    -- * 'InputStdin' then this is interpreted as write into @stdout@,
    -- * and if it's 'InputFile' then this is means that the input file will be
    --   modified in palace.
    | OutputFile FilePath
  deriving (Show)

instance IsOutput Output where
    parseOutput s = OutputFile <$> Data.Output.parseOutput s

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

readExpression :: Input -> IO (Expr Src Import, Dhall.Import.Status IO)
readExpression = \case
    InputStdin ->
        Text.getContents >>= parseExpr "(stdin)" "."

    InputFile file ->
        Text.readFile file >>= parseExpr file file
  where
    parseExpr f c txt =
        (,) <$> Dhall.Core.throws (Dhall.Parser.exprFromText f txt)
            <*> pure (Dhall.Import.emptyStatus c)

withOutputHandle :: Input -> Output -> (Handle -> b -> IO a) -> b -> IO a
withOutputHandle input = \case
    OutputStdout ->
        ($ stdout)

    OutputBasedOnInput -> case input of
        InputStdin ->
            ($ stdout)

        InputFile filePath ->
            -- TODO: Consider using a temporary file so that this operation can
            -- be perceived as atomic from the outside.
            \m a -> withFile filePath WriteMode \h -> m h a

    OutputFile filePath ->
        \m a -> withFile filePath WriteMode \h -> m h a

-- }}} Input/Output -----------------------------------------------------------

-- {{{ Helper Functions -------------------------------------------------------

renderDoc :: Config -> Handle -> Doc Dhall.Ann -> IO ()
renderDoc Config{colourOutput} h doc =
    Dhall.hPutDoc colourOutput h (doc <> Pretty.line)

hPutExpr :: Config -> Handle -> Expr Src X -> IO ()
hPutExpr Config{colourOutput} = Dhall.hPutExpr colourOutput characterSet
  where
    characterSet = Dhall.Unicode -- TODO: This should be configurable.

data DhallException a
    = PlainDhallException String String a
    | ColourfulDhallException String String a

instance (Show e, Typeable e) => Exception (DhallException e)

instance Show e => Show (DhallException e) where
    show = \case
        PlainDhallException usedName prefix e ->
            prefix <> convertErrorMsg usedName False (show e)

        ColourfulDhallException usedName prefix e ->
            prefix <> convertErrorMsg usedName True (show e)
      where
        -- "\ESC[1;31mError\ESC[0m:" ->
        --     ... <> usedName <> " config: Error: " <> ...
        convertErrorMsg usedName colour s =
            let (prefix, msg) =
                    List.span (/= '\x1b') (List.dropWhile (== '\n') s)

                (msg1, msg2) =
                    List.span (/= '\n') (List.dropWhile (/= ':') msg)

             in mconcat
                    [ prefix
                    , if colour then "\ESC[1;31m" else ""
                    , usedName
                    , " config: Error"
                    , msg1
                    , if colour then "\ESC[0m" else ""
                    , msg2
                    ]

throwDhallException
    :: Exception e
    => AppNames
    -> Config
    -> Handle
    -> String
    -> e
    -> IO a
throwDhallException AppNames{usedName} Config{colourOutput} h msg e = do
    useColours <- shouldUseColours h colourOutput

    let prefix =
            if null msg
                then ""
                else if useColours
                    then
                        "\ESC[2m" <> usedName <> " config: " <> msg
                        <> "\ESC[0m\n\n"
                    else
                        msg <> "\n\n"

    throwIO if useColours
        then ColourfulDhallException usedName prefix e
        else PlainDhallException usedName prefix e

handleExceptions :: AppNames -> Config -> IO () -> IO ()
handleExceptions appNames config@Config{verbosity} =
    handle handlerFinal . handle handler3 . handle handler2 . handle handler1
    . handle handler0
  where
    explain = verbosity > Verbosity.Normal

    handler0 :: TypeError Src X -> IO ()
    handler0 e = if explain
        then throwDhallException' "" (DetailedTypeError e)
        else throwDhallException' getDetailedErrorsMsg e

    handler1 :: Imported (TypeError Src X) -> IO ()
    handler1 (Imported ps e) = if explain
        then throwDhallException' "" (Imported ps (DetailedTypeError e))
        else throwDhallException' getDetailedErrorsMsg (Imported ps e)

    getDetailedErrorsMsg = "Use \"--verbosity=verbose\" for detailed errors."

    handler2 :: SourcedException MissingImports -> IO ()
    handler2 = throwDhallException' ""

    handler3 :: ParseError -> IO ()
    handler3 = throwDhallException' ""

    handlerFinal :: SomeException -> IO ()
    handlerFinal e = do
        let string = show e

        unless (verbosity == Verbosity.Silent || null string)
            (hPutStrLn stderr string)

        System.Exit.exitFailure

    throwDhallException' :: Exception e => String -> e -> IO a
    throwDhallException' = throwDhallException appNames config stderr

-- }}} Helper Functions -------------------------------------------------------
