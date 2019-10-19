{-# LANGUAGE DataKinds #-}
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
    , Format(..)
    , defFormat
    , format

    -- * Freeze
    , Freeze(..)
    , defFreeze
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

    -- * Variable
    , Variable(..)
    , addVariable

    -- * Helpers
    , setAllowImports
    , handleExceptions
    )
  where

import Prelude hiding (filter)

import Control.Exception (Exception, SomeException, bracket, handle, throwIO)
import Control.Monad (unless)
import Data.Foldable (foldrM, for_, traverse_)
import Data.Functor ((<&>))
import Data.List as List (dropWhile, span)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Monoid ((<>))
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import qualified GHC.IO.Encoding as IO (setLocaleEncoding)
import System.Exit (exitFailure)
import System.IO
    ( Handle
    , IOMode(WriteMode)
    , SeekMode(AbsoluteSeek)
    , hClose
    , hSeek
    , hPutStrLn
    , stderr
    , stdout
    , withFile
    )
import qualified System.IO as IO (utf8)

import Crypto.Hash (Digest, SHA256)
import qualified Crypto.Hash as Crypto (hash)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString (hPutStr)
import Data.Either.Validation (validationToEither)
import Data.Generics.Internal.VL.Lens (set, view)
import Data.Generics.Product.Fields (HasField', field')
import Data.Generics.Product.Typed (HasType, typed)
import Data.Output (IsOutput, HasOutput)
import qualified Data.Output (IsOutput(..), HasOutput(..))
import Data.Text (Text)
import qualified Data.Text as Text (unlines, unpack)
import qualified Data.Text.Encoding as Text (encodeUtf8)
import qualified Data.Text.IO as Text
    ( hPutStr
    , hPutStrLn
    , getContents
    , readFile
    )
import Data.Text.Prettyprint.Doc (Doc)
import qualified Data.Map as Map (keys)
import qualified Dhall.Bash as Bash
    ( ExpressionError
    , StatementError
    , dhallToExpression
    , dhallToStatement
    )
import Dhall.Core (Expr(..), Import)
import qualified Dhall.Freeze as Dhall (freezeImport, freezeRemoteImport)
import Dhall.Import (Chained(chainedImport), Imported(..), MissingImports)
import qualified Dhall.Lint as Dhall (lint)
import Dhall.Parser (ParseError, SourcedException, Src)
import qualified Dhall.Pretty as Dhall (Ann, CharacterSet(..))
import Dhall.TypeCheck (DetailedTypeError(..), TypeError, X)
import qualified Dhall.Util as Dhall
  ( Censor(NoCensor)
  , Input(File, StandardInput)
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
import System.FilePath.Parse (parseFilePath)
import System.IO.LockFile.Internal
    ( LockingException(..)
    , LockingParameters(LockingParameters)
    , RetryStrategy(No)
    , lock
    )
import System.Posix.Process (executeFile)

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
    { annotate = False
    , alpha = False
    , allowImports = True
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
  Interpreter{allowImports, alpha, annotate, input, output, showType, variables}
  = handleExceptions appNames config do

    IO.setLocaleEncoding IO.utf8
    (resolvedExpression, inferredType) <- do
        expression <- readExpression input
            >>= resolveImports
            >>= doTheLetThing allowImports variables

        expressionType <- Dhall.Core.throws (Dhall.TypeCheck.typeOf expression)

        if showType
            then
                (expressionType,) <$> Dhall.Core.throws
                    (Dhall.TypeCheck.typeOf expressionType)
            else
                pure (expression, expressionType)

    let normalizedExpression = Dhall.Core.normalize resolvedExpression

        alphaNormalizedExpression =
            if alpha
                then Dhall.Core.alphaNormalize normalizedExpression
                else normalizedExpression

        annotatedExpression =
            if annotate
                then Annot alphaNormalizedExpression inferredType
                else alphaNormalizedExpression

    withOutputHandle input output (hPutExpr config) annotatedExpression
  where
    resolveImports
        :: (Expr Src Import, Dhall.Import.Status)
        -> IO (Expr Src X)
    resolveImports (expr, status)
      | allowImports = State.evalStateT (Dhall.Import.loadWith expr) status
      | otherwise    = Dhall.Import.assertNoImports expr

-- }}} Interpreter ------------------------------------------------------------

-- {{{ Resolve ----------------------------------------------------------------

data Resolve = Resolve
    { mode :: ResolveMode
    , input :: Input
    , output :: Output
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
--                  . Dhall.Core.importType
                    . Dhall.Core.importHashed
                    . chainedImport
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
  deriving stock (Generic, Show)
  deriving anyclass (HasInput)

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

            InputExpression expr ->
                parseExpr "(expression)" expr

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

data Format = Format
    { input :: Input
    }
  deriving stock (Generic, Show)
  deriving anyclass (HasInput)

defFormat :: Format
defFormat = Format
    { input = InputStdin
    }

format :: AppNames -> Config -> Format -> IO ()
format appNames config Format{..} =
    handleExceptions appNames config $ Dhall.Format.format Dhall.Format.Format
        { characterSet = Dhall.Unicode
        , censor = Dhall.NoCensor
        , formatMode = Dhall.Format.Modify case input of
            InputStdin ->
                Dhall.StandardInput

            InputFile file ->
                Dhall.File file

            InputExpression _ ->
                -- TODO: Is there a way how we can handle this gracefully?
                undefined
        }

-- }}} Format -----------------------------------------------------------------

-- {{{ Freeze -----------------------------------------------------------------

data Freeze = Freeze
    { remoteOnly :: Bool
    , input :: Input
    , output :: Output
    , characterSet :: Dhall.CharacterSet
    }
  deriving stock (Generic, Show)
  deriving anyclass (HasInput)

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

        InputExpression expr -> do
            (header, expression) <- parseExpr "(expression)" expr
            pure (header, expression, ".")

    let freezeFunction =
            ( if remoteOnly
                then Dhall.freezeRemoteImport
                else Dhall.freezeImport
            ) directory

    frozenExpression <- traverse freezeFunction expression
    withOutputHandle input output (renderDoc config)
        ( Pretty.pretty header
        <> Dhall.Pretty.prettyCharacterSet characterSet frozenExpression
        )
  where
    parseExpr f = Dhall.Core.throws . Dhall.Parser.exprAndHeaderFromText f

-- }}} Freeze -----------------------------------------------------------------

-- {{{ Hash -------------------------------------------------------------------

data Hash = Hash
    { input :: Input
    , output :: Output
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
    }

hash :: AppNames -> Config -> Hash -> IO ()
hash appNames config Hash{..} = handleExceptions appNames config do

    IO.setLocaleEncoding IO.utf8
    (expression, status) <- readExpression input

    resolvedExpression <- State.evalStateT
        (Dhall.Import.loadWith expression) status

    _ <- Dhall.Core.throws (Dhall.TypeCheck.typeOf resolvedExpression)

    let normalizedExpression =
            Dhall.Core.alphaNormalize (Dhall.Core.normalize resolvedExpression)

    withOutputHandle input output Text.hPutStrLn
        (Dhall.Import.hashExpressionToCode normalizedExpression)

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

deriving stock instance Show Dhall.CharacterSet -- TODO Get rid of orphan

defRepl :: Repl
defRepl = Repl
    { characterSet = Dhall.Unicode
    , historyFile = Nothing
    }

repl :: AppNames -> Config -> Repl -> IO ()
repl appNames config@Config{verbosity} Repl{..} =
    handleExceptions appNames config
        (Dhall.Repl.repl characterSet explain)
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
    }
  deriving stock (Generic, Show)
  deriving anyclass (HasInput)

defExec :: Input -> Exec
defExec input = Exec
    { input
    , interpret = Nothing
    , arguments = []
    }

exec :: AppNames -> Config -> Exec -> IO ()
exec appNames@AppNames{usedName} config Exec{..} =
    handleExceptions appNames config do
        cacheDir <- getXdgDirectory XdgCache (usedName <> "-dhall-exec")
        expression <- case input of
            InputExpression e -> pure e
            _ -> undefined

        content <- Dhall.input Dhall.auto expression
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
    { input = InputStdin
    , allowImports = True
    , mode = BashExpressionMode
    , output = OutputStdout
    }

bash :: AppNames -> Config -> Bash -> IO ()
bash appNames config Bash{..} = handleExceptions appNames config do

    IO.setLocaleEncoding IO.utf8
    (expression, status) <- readExpression input

    resolvedExpression <- do
        expr <- if allowImports
            then
                State.evalStateT (Dhall.Import.loadWith expression) status
            else
                Dhall.Import.assertNoImports expression

        _ <- Dhall.Core.throws (Dhall.TypeCheck.typeOf expr)

        pure expr

    r <- case mode of
        BashExpressionMode ->
            Dhall.Core.throws (Bash.dhallToExpression resolvedExpression)

        BashStatementMode name ->
            Dhall.Core.throws (Bash.dhallToStatement resolvedExpression name)

    withOutputHandle input output ByteString.hPutStr r

-- }}} Bash -------------------------------------------------------------------

-- {{{ To Text ----------------------------------------------------------------

data ToTextMode = PlainTextMode | ListTextMode
  deriving stock (Generic, Show)

data ToText = ToText
    { input :: Input
    , output :: Output
    , allowImports :: Bool
    , mode :: ToTextMode
    }
  deriving stock (Generic, Show)
  deriving anyclass (HasInput)

instance HasOutput ToText where
    type Output ToText = Output
    output = field' @"output"

defToText :: ToText
defToText = ToText
    { input = InputStdin
    , output = OutputStdout
    , allowImports = True
    , mode = PlainTextMode
    }

toText :: AppNames -> Config -> ToText -> IO ()
toText appNames config ToText{..} = handleExceptions appNames config do

    IO.setLocaleEncoding IO.utf8
    (expression, status) <- readExpression input

    let dhallInput :: Dhall.Type a -> IO a
        dhallInput Dhall.Type{..} = do
            expr <- if allowImports
                then
                    State.evalStateT (Dhall.Import.loadWith expression) status
                else
                    Dhall.Import.assertNoImports expression

            _ <- Dhall.Core.throws
                (Dhall.TypeCheck.typeOf (Dhall.Core.Annot expr expected))

            Dhall.Core.throws
                $ validationToEither (extract (Dhall.Core.normalize expr))

    case mode of
        PlainTextMode ->
            dhallInput (Dhall.auto @Text)
            >>= withOutputHandle' Text.hPutStr

        ListTextMode ->
            dhallInput (Dhall.auto @[Text])
            >>= withOutputHandle' (\h -> Text.hPutStr h . Text.unlines)
  where
    withOutputHandle' :: (Handle -> a -> IO ()) -> a -> IO ()
    withOutputHandle' = withOutputHandle input output

-- }}} To Text ----------------------------------------------------------------

-- {{{ Filter -----------------------------------------------------------------

data Filter = Filter
    { input :: Input
    , output :: Output
    , expression :: Text
    , allowImports :: Bool
    , variables :: [Variable]
    }
  deriving stock (Generic, Show)
  deriving anyclass (HasInput)

instance HasOutput Filter where
    type Output Filter = Output
    output = field' @"output"

defFilter :: Text -> Filter
defFilter expression = Filter
    { input = InputStdin
    , output = OutputStdout
    , expression
    , allowImports = True
    , variables = []
    }

filter :: AppNames -> Config -> Filter -> IO ()
filter appNames config Filter{..} = handleExceptions appNames config do

    IO.setLocaleEncoding IO.utf8

    (inVar, inVarType) <- readExpression input >>= resolveImports allowImports
        >>= \value -> do
            annotation <- typeOf value
            pure
                ( Dhall.Core.Binding
                    { variable = "input"
                    , annotation = Just (Nothing, annotation)
                    , value
                    , bindingSrc0 = Nothing
                    , bindingSrc1 = Nothing
                    , bindingSrc2 = Nothing
                    }
                , Dhall.Core.Binding
                    { variable = "Input"
                    , annotation =
                        Just (Nothing, Dhall.Core.Const Dhall.Core.Type)
                    , value = annotation
                    , bindingSrc0 = Nothing
                    , bindingSrc1 = Nothing
                    , bindingSrc2 = Nothing
                    }
                )

    parseExpression expression >>= \expr -> do
        expr' <- Dhall.Core.Let inVar . Dhall.Core.Let inVarType
            <$> resolveImports True (expr, Dhall.Import.emptyStatus ".")
        expr'' <- doTheLetThing allowImports variables expr'
        _ <- typeOf expr''
        withOutputHandle' (hPutExpr config) (Dhall.Core.normalize expr'')
  where
    withOutputHandle' :: (Handle -> a -> IO ()) -> a -> IO ()
    withOutputHandle' = withOutputHandle input output

    typeOf = Dhall.Core.throws . Dhall.TypeCheck.typeOf

    resolveImports
        :: Bool
        -> (Expr Src Import, Dhall.Import.Status)
        -> IO (Expr Src X)
    resolveImports allowImports' (expr, status)
      | allowImports' = State.evalStateT (Dhall.Import.loadWith expr) status
      | otherwise     = Dhall.Import.assertNoImports expr

    parseExpression =
        Dhall.Core.throws . Dhall.Parser.exprFromText "(expression)"

-- }}} Filter -----------------------------------------------------------------

-- {{{ Input/Output -----------------------------------------------------------

data Input
    = InputStdin
    | InputFile FilePath
    | InputExpression Text
  deriving stock (Show)

-- TODO: This is a generic pattern, like HasOutput, it should be moved to
-- mainplate?
class IsInput a where
    parseInput :: String -> Either String a

instance IsInput FilePath where
    parseInput = parseFilePath

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

instance IsOutput Output where
    parseOutput s = OutputFile <$> Data.Output.parseOutput s

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

readExpression :: Input -> IO (Expr Src Import, Dhall.Import.Status)
readExpression = \case
    InputStdin ->
        Text.getContents >>= parseExpr "(stdin)" "."

    InputFile file ->
        Text.readFile file >>= parseExpr file (takeDirectory file)

    InputExpression expr ->
        parseExpr "(expression)" "." expr
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

        InputExpression _ ->
            ($ stdout)

    OutputFile filePath ->
        \m a -> withFile filePath WriteMode \h -> m h a

-- }}} Input/Output -----------------------------------------------------------

-- {{{ Helper Functions -------------------------------------------------------

setAllowImports :: HasField' "allowImports" a Bool => Bool -> a -> a
setAllowImports = set (field' @"allowImports")

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
    handle handlerFinal . handle handler5 . handle handler4 . handle handler3
    . handle handler2 . handle handler1 . handle handler0
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

    handler4 :: Bash.ExpressionError -> IO ()
    handler4 = throwDhallException' ""

    handler5 :: Bash.StatementError -> IO ()
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

doTheLetThing :: Bool -> [Variable] -> Expr Src X -> IO (Expr Src X)
doTheLetThing allowImports = flip (foldrM let_)
  where
    let_ :: Variable -> Expr Src X -> IO (Expr Src X)
    let_ Variable{variable, value = valueText} body = do
        -- We are parsing and resolving imports for each variable separately.
        -- It may be more efficient to do so once for the whole expression.
        parseExpression valueText >>= \expr -> do
            value <- resolveImports expr
            _ <- typeOf value
            pure
                ( Dhall.Core.Binding
                    { variable
                    , annotation = Nothing
                    , value
                    , bindingSrc0 = Nothing
                    , bindingSrc1 = Nothing
                    , bindingSrc2 = Nothing
                    }
                `Dhall.Core.Let` body
                )

    parseExpression =
        -- TODO: "(expression)" should mention variable name.
        Dhall.Core.throws . Dhall.Parser.exprFromText "(expression)"

    resolveImports
        :: Expr Src Import
        -> IO (Expr Src X)
    resolveImports expr
      | allowImports = State.evalStateT (Dhall.Import.loadWith expr) status
      | otherwise    = Dhall.Import.assertNoImports expr
      where
        status = Dhall.Import.emptyStatus "."

    typeOf = Dhall.Core.throws . Dhall.TypeCheck.typeOf

-- }}} Helper Functions -------------------------------------------------------
