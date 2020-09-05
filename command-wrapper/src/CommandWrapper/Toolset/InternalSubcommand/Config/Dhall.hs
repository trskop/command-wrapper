{-# LANGUAGE DataKinds #-}
-- |
-- Module:      $Header$
-- Description: Dhall operations provided by the config command.
-- Copyright:   (c) 2018-2019 Gabriel Gonzalez and contrubutors;
--              (c) 2019-2020 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Dhall operations provided by the config command.  This module started its
-- life as a copy of @Dhall.Main@ from @dhall-haskell@ package, hence the
-- copyright.
module CommandWrapper.Toolset.InternalSubcommand.Config.Dhall
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
    , Format(..)
    , defFormat
    , format

    -- * Freeze
    , Freeze(..)
    , FreezePurpose(..)
    , defFreeze
    , setFreezePurpose
    , freeze

    -- * Hash
    , Hash(..)
    , defHash
    , hash

    -- * Diff
    , Diff(..)
    , defDiff
    , diff

    -- * REPL
    , Repl(..)
    , defRepl
    , repl

    -- * Exec
    , Exec(..)
    , defExec
    , exec

    -- * Bash
    , Bash(..)
    , BashMode(..)
    , defBash
    , bash

    -- * To Text
    , ToText(..)
    , ToTextMode(..)
    , defToText
    , toText

    -- * Filter
    , Filter(..)
    , defFilter
    , filter

    -- * Input\/Output
    , Input(..)
    , HasInput(..)
    , IsInput(..)
    , setInput

    , Output(..)
    , OutputOrCheck(..)
    , ReportOutput(..)

    -- * Cache
    , SemanticCacheMode(..)
    , setSemanticCacheMode

    -- * Variable
    , Variable(..)
    , addVariable

    -- * Helpers
    , hPutExpr
    , handleExceptions
    , setAllowImports
    , setOnlySecureImpors
    , setAlpha
    , setAnnotate
    , setShowType
    )
  where

import Prelude (error)

import Control.Applicative ((<*>), pure)
import Control.Exception (Exception, SomeException, bracket, handle, throwIO)
import Control.Monad ((=<<), (>>=), unless, when)
import Data.Bool (Bool(False, True), (||), otherwise)
import Data.Char (Char)
import Data.Eq (Eq, (/=), (==))
import Data.Foldable (foldrM, for_, null, traverse_)
import Data.Functor (Functor, (<$>), (<&>), void)
import Data.Function (($), (.), flip, id)
import Data.List as List (dropWhile, map, span)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Monoid ((<>), mconcat)
import Data.Ord ((>))
import Data.String (String)
import Data.Traversable (traverse)
import Data.Tuple (uncurry)
import Data.Typeable (Typeable)
import Data.Void (Void)
import GHC.Generics (Generic)
import qualified GHC.IO.Encoding as IO (setLocaleEncoding)
import System.Exit (exitFailure)
import System.IO
    ( FilePath
    , Handle
    , IO
    , IOMode(WriteMode)
    , SeekMode(AbsoluteSeek)
    , hClose
    , hPutStrLn
    , hSeek
    , print -- TODO: Get rid of this dependency, everything should support
            -- custom output.
    , stderr
    , stdout
    , withFile
    )
import qualified System.IO as IO (utf8)
import Text.Show (Show, show)

import qualified Control.Monad.Trans.State.Strict as State
import Crypto.Hash (Digest, SHA256)
import qualified Crypto.Hash as Crypto (hash)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString (hPutStr)
import Data.Either.Validation (validationToEither)
import Data.Generics.Internal.VL.Lens ((^.), set, view)
import Data.Generics.Product.Fields (HasField', field')
import Data.Generics.Product.Typed (HasType, typed)
import Data.Output (IsOutput, HasOutput)
import qualified Data.Output (IsOutput(..), HasOutput(..))
import Data.Text (Text)
import qualified Data.Text as Text
    ( concat
    , intercalate
    , singleton
    , snoc
    , unlines
    , unpack
    , unwords
    )
import qualified Data.Text.Encoding as Text (encodeUtf8)
import qualified Data.Text.IO as Text
    ( hPutStr
    , hPutStrLn
    , getContents
    , readFile
    )
import Data.Text.Prettyprint.Doc (Doc, pretty)
import qualified Data.Text.Prettyprint.Doc as Pretty (line)
import qualified Data.Text.Prettyprint.Doc.Render.Text as Pretty (renderStrict)
import qualified Dhall
    ( Decoder(Decoder, expected, extract)
    , auto
    , inputExpr
    )
import qualified Dhall.Bash as Dhall.Bash
    ( ExpressionError
    , StatementError
    , dhallToExpression
    , dhallToStatement
    )
import qualified Dhall.Core as Dhall
    ( Binding
        ( Binding
        , annotation
        , bindingSrc0
        , bindingSrc1
        , bindingSrc2
        , value
        , variable
        )
    , Expr
        ( Annot
        , Embed
        , ImportAlt
        , Let
        , Note
        )
    , Import(..)
    , ImportHashed(..)
    , ImportType(Remote)
    , alphaNormalize
    , denote
    , normalize
    , subExpressions
    , throws
    )
import qualified Dhall.Diff (Diff(Diff, doc), diffNormalized)
import qualified Dhall.Freeze as Dhall (freezeImport, freezeRemoteImport)
import qualified Dhall.Import as Dhall
    ( Chained(chainedImport)
    , Imported(Imported)
    , MissingImports
    , Status(_semanticCacheMode)
    , SemanticCacheMode(IgnoreSemanticCache, UseSemanticCache)
    , assertNoImports
    , cache
    , emptyStatus
    , hashExpressionToCode
    , loadWith
    )
import qualified Dhall.Lint as Dhall (lint)
import qualified Dhall.Map (keys)
import qualified Dhall.Optics (transformMOf)
import qualified Dhall.Parser as Dhall
    ( Header(Header)
    , ParseError
    , SourcedException
    , Src(Src, srcText)
    , exprAndHeaderFromText
    , exprFromText
    )
import qualified Dhall.Pretty as Dhall
    ( Ann
    , CharacterSet(Unicode)
    , layout
    , prettyCharacterSet
    )
import qualified Dhall.Repl as Dhall (repl)
import qualified Dhall.TypeCheck as Dhall
    ( DetailedTypeError(DetailedTypeError)
    , TypeError
    , typeOf
    )
import System.Directory
    ( XdgDirectory(XdgCache)
    , createDirectoryIfMissing
    , emptyPermissions
    , getPermissions
    , getXdgDirectory
    , setOwnerExecutable
    , setOwnerReadable
    , setOwnerSearchable
    , setOwnerWritable
    , setPermissions
    )
import System.FilePath ((</>), takeDirectory)
import System.IO.LockFile.Internal
    ( LockingException(..)
    , LockingParameters(LockingParameters)
    , RetryStrategy(No)
    , lock
    )
import System.Posix.Process (executeFile)

import CommandWrapper.Core.Config.ColourOutput (shouldUseColours)
import qualified CommandWrapper.Core.Config.Verbosity as Verbosity
    ( Verbosity(Normal, Silent)
    )
import qualified CommandWrapper.Core.Dhall as Dhall
    ( assertSecureImports
    , hPutDoc
    , hPutExpr
    )
import CommandWrapper.Core.Environment (AppNames(AppNames, usedName))
import CommandWrapper.Toolset.Config.Global
    ( Config(Config, colourOutput, verbosity)
    )
import CommandWrapper.Toolset.InternalSubcommand.Config.IsInput (IsInput(..))


-- {{{ Interpreter ------------------------------------------------------------

data Interpreter = Interpreter
    { allowImports :: Bool
    , onlySecureImpors :: Bool
    , alpha :: Bool
    , annotate :: Bool
    , showType :: Bool
    , semanticCache :: SemanticCacheMode

    , variables :: [Variable]

--  , inputEncoding   :: InputEncoding
--  , outputEncoding  :: OutputEncoding

    , input :: Input
    , output :: Output
    }
  deriving stock (Generic, Show)
  deriving anyclass (HasInput)

instance HasOutput Interpreter where
    type Output Interpreter = Output
    output = field' @"output"

defInterpreter :: Interpreter
defInterpreter = Interpreter
    { allowImports = True
    , alpha = False
    , annotate = False
    , onlySecureImpors = False
    , semanticCache = UseSemanticCache
    , showType = False

    , variables = []

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
  Interpreter
    { allowImports
    , alpha
    , annotate
    , input
    , onlySecureImpors
    , output
    , semanticCache
    , showType
    , variables
    }
  = handleExceptions appNames config do
    IO.setLocaleEncoding IO.utf8

    (resolvedExpression, inferredType) <- do
        expression <- readExpression semanticCache input
            >>= resolveImports allowImports onlySecureImpors
            >>= doTheLetThing allowImports onlySecureImpors semanticCache
                    variables

        expressionType <- typeOf expression
        if showType
            then (expressionType,) <$> typeOf expressionType
            else pure (expression, expressionType)

    let alphaNormalizedExpression = alphaNormalize alpha resolvedExpression

        annotatedExpression =
            annotateExpression annotate alphaNormalizedExpression inferredType

    withOutputHandle input output (hPutExpr config) annotatedExpression
  where
    typeOf = Dhall.throws . Dhall.typeOf

-- }}} Interpreter ------------------------------------------------------------

-- {{{ Resolve ----------------------------------------------------------------

data Resolve = Resolve
    { mode :: ResolveMode
    , input :: Input
    , output :: Output
    , semanticCache :: SemanticCacheMode
    }
  deriving stock (Generic, Show)
  deriving anyclass (HasInput)

instance HasOutput Resolve where
    type Output Resolve = Output
    output = field' @"output"

data ResolveMode
    = ResolveDependencies
    | ListTransitiveDependencies
    | ListImmediateDependencies
--  | Dot
  deriving stock (Show)

defResolve :: Resolve
defResolve = Resolve
    { mode = ResolveDependencies
    , input = InputStdin
    , output = OutputStdout
    , semanticCache = UseSemanticCache
    }

resolve :: AppNames -> Config -> Resolve -> IO ()
resolve appNames config Resolve{mode, input, output, semanticCache} =
    handleExceptions appNames config do
        IO.setLocaleEncoding IO.utf8

        (expression, status) <- readExpression semanticCache input

        case mode of
            ResolveDependencies -> do
                (resolvedExpression, _)
                    <- State.runStateT (Dhall.loadWith expression) status

                withOutputHandle input output (hPutExpr config)
                    resolvedExpression

            ListImmediateDependencies ->
                -- TODO: Handle I/O correctly.
                traverse_ (print . pretty . Dhall.importHashed) expression

            ListTransitiveDependencies -> do
                cache <- (^. Dhall.cache)
                    <$> State.execStateT (Dhall.loadWith expression) status

                for_ (Dhall.Map.keys cache)
                    -- TODO: Handle I/O correctly.
                    ( print
                    . pretty
--                  . Dhall.importType
                    . Dhall.importHashed
                    . Dhall.chainedImport
                    )

            -- TODO: Unable to implement at the moment.
--          Just Dot -> pure ()

-- }}} Resolve ----------------------------------------------------------------

-- {{{ Lint -------------------------------------------------------------------

data Lint = Lint
    { input :: Input
    , output :: OutputOrCheck
    , characterSet :: Dhall.CharacterSet
    }
  deriving stock (Generic, Show)
  deriving anyclass (HasInput)

instance HasOutput Lint where
    type Output Lint = OutputOrCheck
    output = field' @"output"

defLint :: Lint
defLint = Lint
    { input = InputStdin
    , output = Write OutputStdout
    , characterSet = Dhall.Unicode
    }

lint :: AppNames -> Config -> Lint -> IO ()
lint appNames config Lint{input, output, characterSet} =
    handleExceptions appNames config do
        IO.setLocaleEncoding IO.utf8

        (Dhall.Header header, expression, _) <- readExpressionAndHeader input

        case output of
            Write o ->
                withOutputHandle input o (renderDoc config)
                    (   pretty header
                    <>  Dhall.prettyCharacterSet characterSet
                            (Dhall.lint expression)
                    )
            Check _ ->
                pure () -- TODO: Implement

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

                    (_, value) <- Dhall.throws (Codec.CBOR.Read.deserialiseFromBytes decoder bytes)

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
                        Dhall.throws (Codec.Serialise.deserialiseOrFail cborBytes)
                    else do
                        Dhall.throws (Codec.Serialise.deserialiseOrFail bytes)

            expression <- Dhall.throws (Dhall.Binary.decodeExpression term)

            let doc = Dhall.prettyCharacterSet characterSet expression

            renderDoc System.IO.stdout doc
-}

-- {{{ Format -----------------------------------------------------------------

data Format = Format
    { input :: Input
    , output :: OutputOrCheck
    , characterSet :: Dhall.CharacterSet
    }
  deriving stock (Generic, Show)
  deriving anyclass (HasInput)

instance HasOutput Format where
    type Output Format = OutputOrCheck
    output = field' @"output"

defFormat :: Format
defFormat = Format
    { input = InputStdin
    , characterSet = Dhall.Unicode
    , output = Write OutputStdout
    }

format :: AppNames -> Config -> Format -> IO ()
format appNames config Format{..} = handleExceptions appNames config do
    IO.setLocaleEncoding IO.utf8

    (Dhall.Header header, expression, _) <- readExpressionAndHeader input

    case output of
        Write o ->
            withOutputHandle input o (renderDoc config)
                ( pretty header
                <> Dhall.prettyCharacterSet characterSet expression
                )
        Check _ ->
            pure () -- TODO: Implement

-- }}} Format -----------------------------------------------------------------

-- {{{ Freeze -----------------------------------------------------------------

data FreezePurpose
    = FreezeForSecurity
    | FreezeForCaching
  deriving stock (Generic, Show)


data Freeze = Freeze
    { remoteOnly :: Bool
    , input :: Input
    , output :: OutputOrCheck
    -- ^ 'Nothing' means that we are not producing any output, only checking.
    , characterSet :: Dhall.CharacterSet
    , purpose :: FreezePurpose
    }
  deriving stock (Generic, Show)
  deriving anyclass (HasInput)

instance HasOutput Freeze where
    type Output Freeze = OutputOrCheck
    output = field' @"output"

-- | Default 'freeze' options.
--
-- @
-- 'defFreeze' = 'Freeze'
--     { 'remoteOnly' = 'True'
--     , 'input' = 'InputStdin'
--     , 'output' = 'Write' 'OutputStdout'
--     , 'characterSet' = 'Dhall.Unicode'
--     , 'purpose' = 'FreezeForSecurity'
--     }
-- @
defFreeze :: Freeze
defFreeze = Freeze
    { remoteOnly = True
    , input = InputStdin
    , output = Write OutputStdout
    , characterSet = Dhall.Unicode
    , purpose = FreezeForSecurity
    }

setFreezePurpose
    :: HasField' "purpose" a FreezePurpose
    => FreezePurpose
    -> a
    -> a
setFreezePurpose = set (field' @"purpose")

freeze
    :: AppNames
    -> Config
    -> Freeze
    -> IO ()
freeze appNames config Freeze{..} = handleExceptions appNames config do
    IO.setLocaleEncoding IO.utf8

    (Dhall.Header header, expression, directory)
        <- readExpressionAndHeader input

    frozenExpression <- case purpose of
        FreezeForSecurity ->
            traverse (freezeFunction directory) expression

        FreezeForCaching  ->
            Dhall.Optics.transformMOf
                Dhall.subExpressions
                (rewriteForCaching directory)
                (Dhall.denote expression)

    case output of
        Write o ->
            withOutputHandle input o (renderDoc config)
                ( pretty header
                <> Dhall.prettyCharacterSet characterSet frozenExpression
                )
        Check _ ->
            pure () -- TODO: Implement
  where
    freezeFunction =
        if remoteOnly
            then Dhall.freezeRemoteImport
            else Dhall.freezeImport

    -- Following code is mostly copy-paste-reformatt from `dhall` library
    -- version 1.27.0.
    rewriteForCaching directory = \case
        Dhall.ImportAlt
            ( Dhall.Embed Dhall.Import
                { importHashed =
                    Dhall.ImportHashed{hash = Just _expectedHash}
                }
            )
            import_@(
                Dhall.ImportAlt
                    ( Dhall.Embed Dhall.Import
                        { importHashed =
                            Dhall.ImportHashed{hash = Just _actualHash}
                        }
                    )
                _
            )
            -> do
                -- Here we could actually compare the `_expectedHash` and
                -- `_actualHash` to see if they differ, but we choose not to do
                -- so and instead automatically accept the `_actualHash`.  This
                -- is done for the same reason that the `freeze*` functions
                -- ignore hash mismatches: the user intention when using `dhall
                -- freeze` is to update the hash, which they expect to possibly
                -- change.
                pure import_

        Dhall.Embed
            import_@(
                Dhall.Import
                    { importHashed = Dhall.ImportHashed{hash = Nothing}
                    }
            )
            -> do
                frozenImport <- freezeFunction directory import_

                -- The two imports can be the same if the import is local and
                -- `freezeFunction` only freezes remote imports
                pure if frozenImport /= import_
                    then
                        Dhall.ImportAlt
                            (Dhall.Embed frozenImport)
                            (Dhall.Embed import_)
                    else
                        Dhall.Embed import_

        expression ->
            pure expression

-- }}} Freeze -----------------------------------------------------------------

-- {{{ Hash -------------------------------------------------------------------

data Hash = Hash
    { input :: Input
    , output :: Output
    , semanticCache :: SemanticCacheMode
    }
  deriving stock (Generic, Show)
  deriving anyclass (HasInput)

instance HasOutput Hash where
    type Output Hash = Output
    output = field' @"output"

defHash :: Hash
defHash = Hash
    { input = InputStdin
    , output = OutputStdout
    , semanticCache = UseSemanticCache
    }

hash :: AppNames -> Config -> Hash -> IO ()
hash appNames config Hash{..} = handleExceptions appNames config do
    IO.setLocaleEncoding IO.utf8

    (expression, status) <- readExpression semanticCache input
    resolvedExpression <- State.evalStateT (Dhall.loadWith expression) status
    _ <- Dhall.throws (Dhall.typeOf resolvedExpression)

    withOutputHandle input output Text.hPutStrLn
        (Dhall.hashExpressionToCode (alphaNormalize True resolvedExpression))

-- }}} Hash -------------------------------------------------------------------

-- {{{ Diff -------------------------------------------------------------------

data Diff = Diff
    { expr1 :: Text
    , expr2 :: Text
    , output :: Output
    }
  deriving stock (Generic, Show)

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
    Dhall.Diff.Diff{doc} <- Dhall.Diff.diffNormalized
        <$> Dhall.inputExpr expr1
        <*> Dhall.inputExpr expr2

    withOutputHandle InputStdin output (renderDoc config) doc

-- }}} Diff -------------------------------------------------------------------

-- {{{ REPL -------------------------------------------------------------------

data Repl = Repl
    { characterSet :: Dhall.CharacterSet

    , historyFile :: Maybe FilePath -- TODO Is this a correct type?
    -- ^ This is not currently supported by neither @dhall@ library nor by
    -- @repline@ which is used by @dhall@ to implement REPL functionality.
    }
  deriving stock (Show)

defRepl :: Repl
defRepl = Repl
    { characterSet = Dhall.Unicode
    , historyFile = Nothing
    }

repl :: AppNames -> Config -> Repl -> IO ()
repl appNames config@Config{verbosity} Repl{..} =
    handleExceptions appNames config
        (Dhall.repl characterSet explain)
  where
    explain = verbosity > Verbosity.Normal

-- }}} REPL -------------------------------------------------------------------

-- {{{ Exec -------------------------------------------------------------------

data Exec = Exec
    { input :: Input
    -- ^ Dhall expression.

    , interpret :: Maybe (NonEmpty Text)
    -- ^ Describes how the Dhall expression should be interpreted.
    --
    -- * 'Nothing' - Write expression into a file, set executable bit, and
    --   execute it.
    --
    -- * @'Just' (interpreterCommand ':|' interpreterArguments)@ - Write
    --   expression into a file, and pass it to @interpreterCommand@.

    , arguments :: [Text]
    -- ^ Arguments passed to script.  If an interpreter command is specified
    -- then it is executed as:
    --
    -- > INTERPRETER_COMMAND [INTERPRETER_ARGUMENTS] SCRIPT [SCRIPT_ARGUMENTS]
    --
    -- Where @SCRIPT@ is a file into which 'expression' was written.

    , allowImports :: Bool
    , onlySecureImpors :: Bool
    , semanticCache :: SemanticCacheMode
    }
  deriving stock (Generic, Show)
  deriving anyclass (HasInput)

defExec :: Input -> Exec
defExec input = Exec
    { input
    , interpret = Nothing
    , arguments = []
    , allowImports = True
    , onlySecureImpors = True
    , semanticCache = UseSemanticCache
    }

exec :: AppNames -> Config -> Exec -> IO ()
exec appNames@AppNames{usedName} config Exec{..} =
    handleExceptions appNames config do
        cacheDir <- getXdgDirectory XdgCache (usedName <> "-dhall-exec")

        content <- do
            e <- resolveImports allowImports onlySecureImpors =<< case input of
                InputFile file ->
                    Text.readFile file >>= parseExpr file (takeDirectory file)

                InputExpression expr ->
                    parseExpr "(expression)" "." expr

                InputStdin ->
                    error (usedName <> "dhall-exec: Impossible case")

            let Dhall.Decoder{expected, extract} = Dhall.auto @Text

            expected' <- Dhall.throws (validationToEither expected)

            let expectedText =
                    Pretty.renderStrict (Dhall.layout (pretty expected'))

            _ <- Dhall.throws $ Dhall.typeOf case e of
                    Dhall.Note src@Dhall.Src{srcText} _ ->
                        Dhall.Note
                            src { Dhall.srcText =
                                    srcText <> " : " <> expectedText
                                }
                            (Dhall.Annot e expected')
                    _ ->
                        Dhall.Annot e expected'

            Dhall.throws $ validationToEither (extract (Dhall.normalize e))

        let checksum = Crypto.hash (Text.encodeUtf8 content)
        withScript cacheDir checksum \executable possiblyHandle -> do
            for_ possiblyHandle \h -> do
                -- TODO: We can get rid of hSeek when we switch to an
                -- implementation that doesn't write PID into the file.
                hSeek h AbsoluteSeek 0
                Text.hPutStr h content

                -- Closing the file flushes the content and releases the
                -- associated resources so that `executeFile` can actually open
                -- it.
                hClose h

            execute =<< case interpret of
                Nothing -> do
                    getPermissions executable
                        >>= setPermissions executable . setOwnerExecutable True

                    pure (executable, Text.unpack <$> arguments, False)

                Just (interpreterCommand :| interpreterArguments) -> do
                    let arguments' = mconcat
                            [ Text.unpack <$> interpreterArguments
                            , [executable]
                            , Text.unpack <$> interpreterArguments
                            ]

                    pure (Text.unpack interpreterCommand, arguments', True)
  where
    execute (cmd, args, searchPath) = executeFile cmd searchPath args Nothing

    parseExpr f c txt =
        (,) <$> Dhall.throws (Dhall.exprFromText f txt)
            <*> pure (mkImportStatus c semanticCache)

    withScript
        :: FilePath
        -> Digest SHA256
        -> (FilePath -> Maybe Handle -> IO a)
        -> IO a
    withScript file checksum action = bracket
        (getScript file checksum)
        (\(_, h) -> maybe (pure ()) hClose h)
        (uncurry action)

    getScript :: FilePath -> Digest SHA256 -> IO (FilePath, Maybe Handle)
    getScript dir checksum = handle lockingException do
        createDirectoryIfMissing True dir
        setPermissions dir
            ( setOwnerReadable   True
            . setOwnerWritable   True
            . setOwnerSearchable True
            $ emptyPermissions
            )

        let file = dir </> show checksum
        lock (LockingParameters No 0) file <&> \h -> (file, Just h)

    lockingException :: LockingException -> IO (FilePath, Maybe Handle)
    lockingException = \case
        UnableToAcquireLockFile file ->
            pure (file, Nothing)

        CaughtIOException e ->
            throwIO e

-- }}} Exec -------------------------------------------------------------------

-- {{{ Bash -------------------------------------------------------------------

data BashMode = BashExpressionMode | BashStatementMode ByteString
  deriving stock (Generic, Show)

data Bash = Bash
    { input :: Input
    , allowImports :: Bool
    , onlySecureImpors :: Bool
    , semanticCache :: SemanticCacheMode
    , mode :: BashMode
    , output :: Output
    }
  deriving stock (Generic, Show)
  deriving anyclass (HasInput)

instance HasOutput Bash where
    type Output Bash = Output
    output = field' @"output"

defBash :: Bash
defBash = Bash
    { allowImports = True
    , input = InputStdin
    , mode = BashExpressionMode
    , onlySecureImpors = False
    , output = OutputStdout
    , semanticCache = UseSemanticCache
    }

bash :: AppNames -> Config -> Bash -> IO ()
bash appNames config Bash{..} = handleExceptions appNames config do

    IO.setLocaleEncoding IO.utf8
    (expression, status) <- readExpression semanticCache input

    resolvedExpression <- do
        e <- resolveImports allowImports onlySecureImpors (expression, status)
        _ <- Dhall.throws (Dhall.typeOf e)
        pure e

    r <- case mode of
        BashExpressionMode ->
            Dhall.throws (Dhall.Bash.dhallToExpression resolvedExpression)

        BashStatementMode name ->
            Dhall.throws (Dhall.Bash.dhallToStatement resolvedExpression name)

    withOutputHandle input output ByteString.hPutStr r

-- }}} Bash -------------------------------------------------------------------

-- {{{ To Text ----------------------------------------------------------------

data ToTextMode = PlainTextMode | ListTextMode
  deriving stock (Generic, Show)

data ToText = ToText
    { input :: Input
    , output :: Output
    , allowImports :: Bool
    , onlySecureImpors :: Bool
    , semanticCache :: SemanticCacheMode
    , mode :: ToTextMode
    , outputDelimiter :: Char
    }
  deriving stock (Generic, Show)
  deriving anyclass (HasInput)

instance HasOutput ToText where
    type Output ToText = Output
    output = field' @"output"

defToText :: ToText
defToText = ToText
    { allowImports = True
    , input = InputStdin
    , mode = PlainTextMode
    , onlySecureImpors = False
    , output = OutputStdout
    , outputDelimiter = '\n'
    , semanticCache = UseSemanticCache
    }

toText :: AppNames -> Config -> ToText -> IO ()
toText appNames config ToText{..} = handleExceptions appNames config do

    IO.setLocaleEncoding IO.utf8
    (expression, status) <- readExpression semanticCache input

    let dhallInput :: Dhall.Decoder a -> IO a
        dhallInput Dhall.Decoder{..} = do
            e <- resolveImports allowImports onlySecureImpors
                (expression, status)
            expected' <- Dhall.throws (validationToEither expected)
            _ <- Dhall.throws (Dhall.typeOf (Dhall.Annot e expected'))
            Dhall.throws $ validationToEither (extract (Dhall.normalize e))

    case mode of
        PlainTextMode ->
            dhallInput (Dhall.auto @Text)
            >>= withOutputHandle' Text.hPutStr

        ListTextMode ->
            dhallInput (Dhall.auto @[Text])
            >>= withOutputHandle' (\h -> Text.hPutStr h . undelimited)
  where
    withOutputHandle' :: (Handle -> a -> IO ()) -> a -> IO ()
    withOutputHandle' = withOutputHandle input output

    undelimited :: [Text] -> Text
    undelimited = case outputDelimiter of
        '\n' -> Text.unlines
        '\0' -> Text.concat . List.map (`Text.snoc` '\0')
        ' ' -> Text.unwords
        d -> Text.intercalate (Text.singleton d)

-- }}} To Text ----------------------------------------------------------------

-- {{{ Filter -----------------------------------------------------------------

data Filter = Filter
    { allowImports :: Bool
    , onlySecureImpors :: Bool
    , alpha :: Bool
    , annotate :: Bool
    , expression :: Text
    , input :: Input
    , output :: Output
    , semanticCache :: SemanticCacheMode
    , showType :: Bool
    , variables :: [Variable]
    }
  deriving stock (Generic, Show)
  deriving anyclass (HasInput)

instance HasOutput Filter where
    type Output Filter = Output
    output = field' @"output"

defFilter :: Text -> Filter
defFilter expression = Filter
    { allowImports = True
    , onlySecureImpors = False
    , alpha = False
    , annotate = False
    , expression
    , input = InputStdin
    , output = OutputStdout
    , semanticCache = UseSemanticCache
    , showType = False
    , variables = []
    }

filter :: AppNames -> Config -> Filter -> IO ()
filter appNames config Filter{..} = handleExceptions appNames config do
    IO.setLocaleEncoding IO.utf8

    (inVar, inVarType) <- readExpression semanticCache input
        >>= resolveImports allowImports onlySecureImpors
        >>= \value -> do
            annotation <- typeOf value
            pure
                ( Dhall.Binding
                    { variable = "input"
                    , annotation = Just (Nothing, annotation)
                    , value
                    , bindingSrc0 = Nothing
                    , bindingSrc1 = Nothing
                    , bindingSrc2 = Nothing
                    }
                , Dhall.Binding
                    { variable = "Input"
                    , annotation = Nothing
                    , value = annotation
                    , bindingSrc0 = Nothing
                    , bindingSrc1 = Nothing
                    , bindingSrc2 = Nothing
                    }
                )

    parseExpression expression >>= \expr -> do
        expr' <- Dhall.Let inVar . Dhall.Let inVarType
            -- TODO: Respect the options here as well? Separate options?
            <$> resolveImports True False (expr, emptyStatus)

        expr''
            <- doTheLetThing allowImports onlySecureImpors semanticCache
                variables expr'

        processExpression expr'' >>= withOutputHandle' (hPutExpr config)
  where
    withOutputHandle' :: (Handle -> a -> IO ()) -> a -> IO ()
    withOutputHandle' = withOutputHandle input output

    typeOf = Dhall.throws . Dhall.typeOf

    processExpression
        :: (Dhall.Expr Dhall.Src Void)
        -> IO (Dhall.Expr Dhall.Src Void)
    processExpression expr = do
        expressionType <- typeOf expr
        (resultExpression, inferredType) <- if showType
            then (expressionType,) <$> typeOf expressionType
            else pure (expr, expressionType)

        let alphaNormalizedExpression = alphaNormalize alpha resultExpression

            annotatedExpression =
                annotateExpression annotate alphaNormalizedExpression
                    inferredType

        pure annotatedExpression

    parseExpression = Dhall.throws . Dhall.exprFromText "(expression)"

    emptyStatus = mkImportStatus "." semanticCache

-- }}} Filter -----------------------------------------------------------------

-- {{{ Input/Output -----------------------------------------------------------

data Input
    = InputStdin
    | InputFile FilePath
    | InputExpression Text
  deriving stock (Show)

instance IsInput Input where
    parseInput s = InputFile <$> parseInput s

-- TODO: This is a generic pattern, like HasOutput, it should be generalised
-- and moved to mainplate?
class HasInput a where
    inputL :: Functor f => (Input -> f Input) -> a -> f a

    default inputL
        :: (HasType Input a, Functor f)
        => (Input -> f Input)
        -> a
        -> f a
    inputL = typed @Input

instance HasInput Input where
    inputL = id

setInput :: HasInput a => Input -> a -> a
setInput = set inputL

data Output
    = OutputStdout
    | OutputBasedOnInput
    -- ^ If 'Input':
    --
    -- * 'InputStdin' then this is interpreted as write into @stdout@,
    -- * and if it's 'InputFile' then this is means that the input file will be
    --   modified in palace.
    | OutputFile FilePath
  deriving stock (Show)

-- | Constructs 'Output' using 'OutputFile' constructor.
instance IsOutput Output where
    parseOutput s = OutputFile <$> Data.Output.parseOutput s

data ReportOutput
    = ReportStdout
    -- ^ Print report to @stdout@.
    | ReportFile FilePath
    -- ^ Write report into a file.
    | ReportNone
    -- ^ Do not print\/write any reporting information. Command can still
    -- indicate success or failure using exit code.
  deriving stock (Show)

-- | Constructs 'ReportOutput' using 'ReportFile' constructor.
instance IsOutput ReportOutput where
    parseOutput s = ReportFile <$> Data.Output.parseOutput s

-- | Write 'Output' or report result of a check to 'ReportOutput'.
data OutputOrCheck
    = Write Output
    -- ^ Write generated output.
    | Check ReportOutput
    -- ^ Instead of writing output perform a check and notify the user
  deriving stock (Show)

-- | Constructs 'OutputOrCheck' using @'Write' . 'OutputFile'@ constructor.
--
-- TODO: Consider removing this instance as making the choice beteween 'Write'
-- and 'Check' should be explicit in the code.
instance IsOutput OutputOrCheck where
    parseOutput s = Write <$> Data.Output.parseOutput s

data InputEncoding
    = InputCbor
    | InputCborJson
    | InputDhall
    | InputJson
    | InputText
    | InputYaml
  deriving stock (Show)

data OutputEncoding
    = OutputCbor
    | OutputCborJson
    | OutputDhallAscii
    | OutputDhallUnicode
    | OutputJson
    | OutputYaml
  deriving stock (Show)

readExpression
    :: SemanticCacheMode
    -> Input
    -> IO (Dhall.Expr Dhall.Src Dhall.Import, Dhall.Status)
readExpression semanticCache = \case
    InputStdin ->
        Text.getContents >>= parseExpr "(stdin)" "."

    InputFile file ->
        Text.readFile file >>= parseExpr file (takeDirectory file)

    InputExpression expr ->
        parseExpr "(expression)" "." expr
  where
    parseExpr f c txt =
        (,) <$> Dhall.throws (Dhall.exprFromText f txt)
            <*> pure (emptyStatus c)

    emptyStatus c = mkImportStatus c semanticCache

readExpressionAndHeader
    :: Input
    -> IO (Dhall.Header, Dhall.Expr Dhall.Src Dhall.Import, FilePath)
readExpressionAndHeader = \case
    InputStdin -> do
        (header, expression) <- Text.getContents
            >>= parseExpr "(stdin)"
        pure (header, expression, ".")

    InputFile file -> do
        (header, expression) <- Text.readFile file
            >>= parseExpr file
        pure (header, expression, takeDirectory file)

    InputExpression expr -> do
        (header, expression) <- parseExpr "(expression)" expr
        pure (header, expression, ".")
  where
    parseExpr f = Dhall.throws . Dhall.exprAndHeaderFromText f

withOutputHandle :: Input -> Output -> (Handle -> b -> IO a) -> b -> IO a
withOutputHandle input = \case
    OutputStdout ->
        ($ stdout)

    OutputBasedOnInput -> case input of
        InputStdin ->
            ($ stdout)

        InputFile filePath ->
            -- TODO: Use atomic version instead.
            \m a -> withFile filePath WriteMode \h -> m h a

        InputExpression _ ->
            ($ stdout)

    OutputFile filePath ->
        -- TODO: Use atomic version instead.
        \m a -> withFile filePath WriteMode \h -> m h a

-- }}} Input/Output -----------------------------------------------------------

-- {{{ Semantic Cache ---------------------------------------------------------

-- | Same data type as 'Dhall.SemanticCacheMode'.
data SemanticCacheMode
    = IgnoreSemanticCache
    | UseSemanticCache
  deriving stock (Generic, Show)

toDhallSemanticCacheMode :: SemanticCacheMode -> Dhall.SemanticCacheMode
toDhallSemanticCacheMode = \case
    IgnoreSemanticCache -> Dhall.IgnoreSemanticCache
    UseSemanticCache    -> Dhall.UseSemanticCache

setSemanticCacheMode
    :: HasField' "semanticCache" a SemanticCacheMode
    => SemanticCacheMode
    -> a
    -> a
setSemanticCacheMode = set (field' @"semanticCache")

-- }}} Semantic Cache ---------------------------------------------------------

-- {{{ Helper Functions -------------------------------------------------------

setAllowImports :: HasField' "allowImports" a Bool => Bool -> a -> a
setAllowImports = set (field' @"allowImports")

setOnlySecureImpors :: HasField' "onlySecureImpors" a Bool => Bool -> a -> a
setOnlySecureImpors = set (field' @"onlySecureImpors")

setAlpha :: HasField' "alpha" a Bool => Bool -> a -> a
setAlpha = set (field' @"alpha")

setAnnotate :: HasField' "annotate" a Bool => Bool -> a -> a
setAnnotate = set (field' @"annotate")

setShowType :: HasField' "showType" a Bool => Bool -> a -> a
setShowType = set (field' @"showType")

renderDoc :: Config -> Handle -> Doc Dhall.Ann -> IO ()
renderDoc Config{colourOutput} h doc =
    Dhall.hPutDoc colourOutput h (doc <> Pretty.line)

hPutExpr :: Config -> Handle -> Dhall.Expr Dhall.Src Void -> IO ()
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
    handle handlerFinal . handle handler5 . handle handler4 . handle handler3
    . handle handler2 . handle handler1 . handle handler0
  where
    explain = verbosity > Verbosity.Normal

    handler0 :: Dhall.TypeError Dhall.Src Void -> IO ()
    handler0 e = if explain
        then
            throwDhallException' "" (Dhall.DetailedTypeError e)
        else
            throwDhallException' getDetailedErrorsMsg e

    handler1 :: Dhall.Imported (Dhall.TypeError Dhall.Src Void) -> IO ()
    handler1 (Dhall.Imported ps e) = if explain
        then
            throwDhallException' ""
                (Dhall.Imported ps (Dhall.DetailedTypeError e))
        else
            throwDhallException' getDetailedErrorsMsg (Dhall.Imported ps e)

    getDetailedErrorsMsg = "Use \"--verbosity=verbose\" for detailed errors."

    handler2 :: Dhall.SourcedException Dhall.MissingImports -> IO ()
    handler2 = throwDhallException' ""

    handler3 :: Dhall.ParseError -> IO ()
    handler3 = throwDhallException' ""

    handler4 :: Dhall.Bash.ExpressionError -> IO ()
    handler4 = throwDhallException' ""

    handler5 :: Dhall.Bash.StatementError -> IO ()
    handler5 = throwDhallException' ""

    handlerFinal :: SomeException -> IO ()
    handlerFinal e = do
        let string = show e

        unless (verbosity == Verbosity.Silent || null string)
            (hPutStrLn stderr string)

        System.Exit.exitFailure

    throwDhallException' :: Exception e => String -> e -> IO a
    throwDhallException' = throwDhallException appNames config stderr

data Variable = Variable
    { variable :: Text
    -- ^ Variable name.
    , value :: Text
    -- ^ Dhall expression forming variable value\/body.
    }
  deriving stock (Eq, Show)

addVariable
    :: forall a
    .  (HasField' "variables" a [Variable])
    => Variable
    -> a
    -> a
addVariable var a =
    set (field' @"variables") (view (field' @"variables") a <> [var]) a

doTheLetThing
    :: Bool
    -> Bool
    -> SemanticCacheMode
    -> [Variable]
    -> Dhall.Expr Dhall.Src Void
    -> IO (Dhall.Expr Dhall.Src Void)
doTheLetThing allowImports onlySecureImpors semanticCache = flip (foldrM let_)
  where
    let_
        :: Variable
        -> Dhall.Expr Dhall.Src Void
        -> IO (Dhall.Expr Dhall.Src Void)
    let_ Variable{variable, value = valueText} body = do
        -- We are parsing and resolving imports for each variable separately.
        -- It may be more efficient to do so once for the whole expression.
        parseExpression valueText >>= \expr -> do
            value <- resolveImports' expr
            _ <- Dhall.throws (Dhall.typeOf value)
            pure
                ( Dhall.Binding
                    { variable
                    , annotation = Nothing
                    , value
                    , bindingSrc0 = Nothing
                    , bindingSrc1 = Nothing
                    , bindingSrc2 = Nothing
                    }
                `Dhall.Let` body
                )

    parseExpression =
        -- TODO: "(expression)" should mention variable name.
        Dhall.throws . Dhall.exprFromText "(expression)"

    resolveImports'
        :: Dhall.Expr Dhall.Src Dhall.Import
        -> IO (Dhall.Expr Dhall.Src Void)
    resolveImports' expr =
        resolveImports allowImports onlySecureImpors (expr, status)
      where
        status = mkImportStatus "." semanticCache

mkImportStatus :: FilePath -> SemanticCacheMode -> Dhall.Status
mkImportStatus path semanticCache = (Dhall.emptyStatus path)
    { Dhall._semanticCacheMode = toDhallSemanticCacheMode semanticCache
    }

resolveImports
    :: Bool
    -> Bool
    -> (Dhall.Expr Dhall.Src Dhall.Import, Dhall.Status)
    -> IO (Dhall.Expr Dhall.Src Void)
resolveImports allowImports onlySecureImpors (expr, status)
  | allowImports = do
        when onlySecureImpors do
            void $ flip Dhall.assertSecureImports expr \case
                Dhall.Remote _ -> True
                _ -> False

        State.evalStateT (Dhall.loadWith expr) status

  | otherwise =
        Dhall.assertNoImports expr

alphaNormalize
    :: Eq a
    => Bool
    -- ^ If 'True' then perform alpha normalisation after normalisation.
    -> Dhall.Expr s a
    -> Dhall.Expr t a
alphaNormalize alpha expr =
    if alpha
        then Dhall.alphaNormalize normalizedExpression
        else normalizedExpression
  where
    normalizedExpression = Dhall.normalize expr

annotateExpression
    :: Bool
    -- ^ Condition that if it's 'True' will cause the expression to be
    -- annotated with a type signature.
    -> Dhall.Expr s a
    -- ^ Type annotation.
    -> Dhall.Expr s a
    -- ^ Expression to be annotated by a type signature if condition holds.
    -> Dhall.Expr s a
annotateExpression annotate expr typeExpr =
    if annotate
        then Dhall.Annot expr typeExpr
        else expr

-- }}} Helper Functions -------------------------------------------------------
