-- |
-- Module:      DhallInput
-- Description: Dhall representation of a command.
-- Copyright:   (c) 2020 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Dhall representation of a command as it is accepted by
-- @--expression=EXPRESSION@ option.
module DhallInput
    (
      DhallInput(..)
    , fromDhallInput

    , DhallInputParams(..)
    , defaultDhallInputParams
    , dhallInput
    )
  where

import Control.Applicative (pure)
import Control.Exception (throwIO)
import Control.Monad ((>>=))
import Data.Bifunctor (first)
import Data.Bool (Bool(True), otherwise)
import Data.Either (Either(Left, Right))
import Data.Foldable (foldr1)
import Data.Function (($), flip)
import Data.Functor ((<$), (<$>), fmap)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Maybe (Maybe(Nothing))
import Data.Semigroup ((<>))
import Data.String (String)
import Data.Void (Void)
import GHC.Generics (Generic)
import System.IO (FilePath, IO)
import Text.Show (Show)

import Data.Either.Validation (validationToEither)
import Data.Text (Text)
import qualified Data.Text as Text (unpack)
import qualified Data.Text.Prettyprint.Doc as Pretty (pretty)
import qualified Data.Text.Prettyprint.Doc.Render.Text as Pretty (renderStrict)
import Data.Verbosity (Verbosity)
import Dhall (FromDhall)
import qualified Dhall
import qualified Dhall.Core as Dhall
    ( Expr(Annot, Note)
    , Import
    , normalize
    , throws
    )
import qualified Dhall.Import as Dhall
    ( SemanticCacheMode(UseSemanticCache)
    , assertNoImports
    , loadRelativeTo
    )
import qualified Dhall.Parser as Dhall (exprFromText)
import qualified Dhall.Pretty as Dhall (layout)
import qualified Dhall.Src as Dhall (Src(Src, srcText))
import qualified Dhall.TypeCheck as Dhall (TypeError, typeOf)

import CommandWrapper.Core.Config.ColourOutput (ColourOutput)
import CommandWrapper.Toolset.Config.Command
    ( Command(..)
    , NamedCommand(..)
    )


data DhallInput
    = MkExecCommandInput (Verbosity -> ColourOutput -> [Text] -> Command)
    -- ^ Corresponds to function that constructs
    -- `CommandWrapper.ExecCommand.Type` in `command` field of
    -- `CommandWrapper.ExecNamedCommand.Type`.
    | ExecNamedCommandInput NamedCommand
    -- ^ Corresponds to `CommandWrapper.ExecNamedCommand.Type`.
    | ExecCommandInput Command
    -- ^ Corresponds to `CommandWrapper.ExecCommand.Type`.
    | CommandInput JustCommand
    -- ^ Corresponds to `CommandWrapper.Command.Type`.

-- | Representation of `CommandWrapper.Command.Type`.
data JustCommand = JustCommand
    { command :: FilePath
    , arguments :: [String]
    }
  deriving stock (Generic, Show)
  deriving anyclass (FromDhall)

fromDhallInput :: DhallInput -> Verbosity -> ColourOutput -> [Text] -> Command
fromDhallInput = \case
    MkExecCommandInput mkCommand ->
        \verbosity colour extraArguments ->
            mkCommand verbosity colour extraArguments
    ExecNamedCommandInput NamedCommand{command = mkCommand} ->
        \verbosity colour extraArguments ->
            mkCommand verbosity colour extraArguments
    ExecCommandInput command@Command{arguments} ->
        \_ _ extraArguments ->
            command{arguments = arguments <> fmap Text.unpack extraArguments}
    CommandInput JustCommand{..} ->
        \_ _ extraArguments ->
            Command
                { command
                , arguments = arguments <> fmap Text.unpack extraArguments
                , environment = []
                , searchPath = True
                , workingDirectory = Nothing
                }

parseDhallInputResolveImportsAndTypeCheck
    :: DhallInputParams
    -> IO (Dhall.Expr Dhall.Src Void)
parseDhallInputResolveImportsAndTypeCheck DhallInputParams{..} =
    -- Reason for type checking so early is to provide reasonable error
    -- messages before attempting to deserialise Haskell data types.  Haskell
    -- data deserialisation is non-deterministic, therefore, the errors
    -- produced there may not be pointing to the real issue.
    parseExpr "(expression)" expression >>= resolveImports "." >>= typeCheck
  where
    parseExpr f txt = Dhall.throws (Dhall.exprFromText f txt)

    resolveImports
        :: FilePath
        -> Dhall.Expr Dhall.Src Dhall.Import
        -> IO (Dhall.Expr Dhall.Src Void)
    resolveImports root
      | allowImports = Dhall.loadRelativeTo root semanticCache
      | otherwise    = Dhall.assertNoImports

    typeCheck :: Dhall.Expr Dhall.Src Void -> IO (Dhall.Expr Dhall.Src Void)
    typeCheck expr =
        Dhall.throws (expr <$ Dhall.typeOf expr)

data Error s
    = TypeError (Dhall.TypeError s Void)
    | ExtractErrors (Dhall.ExtractErrors s Void)

parse
    :: Dhall.Decoder a
    -> Dhall.Expr Dhall.Src Void
    -> Either (Error Dhall.Src) a
parse Dhall.Decoder{..} expr = do
    _ <- first TypeError $ Dhall.typeOf case expr of
        Dhall.Note src@Dhall.Src{srcText} _ ->
            Dhall.Note src{Dhall.srcText = srcText <> " : " <> expectedText}
                (Dhall.Annot expr expected)
        _ ->
            Dhall.Annot expr expected

    first ExtractErrors (validationToEither (extract (Dhall.normalize expr)))
  where
    expectedText = Pretty.renderStrict (Dhall.layout (Pretty.pretty expected))

data DhallInputParams = DhallInputParams
    { expression :: Text
    , allowImports :: Bool
    , semanticCache :: Dhall.SemanticCacheMode
    }

defaultDhallInputParams :: Text -> DhallInputParams
defaultDhallInputParams expression = DhallInputParams
    { expression
    , allowImports = True
    , semanticCache = Dhall.UseSemanticCache
    }

dhallInput :: DhallInputParams -> IO DhallInput
dhallInput input = do
    expr <- parseDhallInputResolveImportsAndTypeCheck input
    throws $ try expr
        ( parseMkExecCommand
        :|  [ parseExecNamedCommand
            , parseExecCommand
            , parseCommand
            ]
        )
  where
    parseMkExecCommand
        :: Dhall.Expr Dhall.Src Void -> Either (Error Dhall.Src) DhallInput
    parseMkExecCommand expr = MkExecCommandInput <$> parse Dhall.auto expr

    parseExecNamedCommand
        :: Dhall.Expr Dhall.Src Void -> Either (Error Dhall.Src) DhallInput
    parseExecNamedCommand expr =
        ExecNamedCommandInput <$> parse Dhall.auto expr

    parseExecCommand
        :: Dhall.Expr Dhall.Src Void -> Either (Error Dhall.Src) DhallInput
    parseExecCommand expr = ExecCommandInput <$> parse Dhall.auto expr

    parseCommand
        :: Dhall.Expr Dhall.Src Void -> Either (Error Dhall.Src) DhallInput
    parseCommand expr = CommandInput <$> parse Dhall.auto expr

    -- TODO: This doesn't provide us with the best error reporting. We'll end
    -- up with the last error, which may not be what user was aiming for.
    try :: Dhall.Expr Dhall.Src Void
        -> NonEmpty
            ( Dhall.Expr Dhall.Src Void
            -> Either (Error Dhall.Src) DhallInput
            )
        -> Either (Error Dhall.Src) DhallInput
    try expr parsers = flip foldr1 (fmap ($ expr) parsers) \a b ->
        case (a, b) of
            (Left _, _)  -> b
            (Right _, _) -> a

    throws :: Either (Error Dhall.Src) a -> IO a
    throws = \case
        Left e -> case e of
            TypeError e' ->
                throwIO e'

            ExtractErrors e' ->
                throwIO e'

        Right a ->
            pure a
