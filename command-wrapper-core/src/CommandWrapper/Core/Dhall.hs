-- |
-- Module:      $Header$
-- Description: Dhall utilities.
-- Copyright:   (c) 2018-2020 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Dhall utilities.
module CommandWrapper.Core.Dhall
    (
    -- * FromDhall Combinators
      interpretWord
    , interpretLazyByteString
    , interpretStrictByteString

    -- ** Unions
    , UnionType(..)
    , constructor
    , constructor0
    , union

    -- * ToDhall Combinators
    , inputString
    , inputList
    , inputMaybe

    -- * I/O
    , hPut
    , hPutExpr
    , hPutDoc

    -- * Secure Imports
    , UnsecureImportResolutionDisabled(..)
    , assertSecureImports
    )
  where

import Prelude (fromIntegral)

import Control.Applicative (empty, pure)
import Control.Exception (Exception)
import Control.Monad (guard)
import Control.Monad.IO.Class (MonadIO)
import Data.Bifunctor (bimap)
import Data.Bool (Bool, not, otherwise)
import Data.Coerce (coerce)
import Data.Either (Either(Left, Right), either)
import Data.Foldable (null)
import Data.Function (($), (.), const)
import Data.Functor (Functor(fmap), (<$>), (<&>))
import Data.Maybe (Maybe(Just, Nothing), fromMaybe, maybe)
import Data.Monoid (Monoid(mempty))
import Data.Semigroup (Semigroup, (<>))
import Data.String (String, fromString)
import Data.Traversable (for)
import Data.Void (Void)
import Data.Word (Word)
import GHC.Exts (IsList(fromList))
import System.IO (Handle, IO)
import Text.Show (Show(show))

import qualified Data.ByteString as Strict (ByteString)
import qualified Data.ByteString.Lazy as Lazy (ByteString)
import Data.Text (Text)
import qualified Data.Text.Encoding as Strict.Text (encodeUtf8)
import qualified Data.Text.Lazy.Encoding as Lazy.Text (encodeUtf8)
import Data.Text.Prettyprint.Doc (Pretty)
import qualified Data.Text.Prettyprint.Doc as Pretty
    ( Doc
    , LayoutOptions(LayoutOptions)
    , PageWidth(AvailablePerLine)
    , layoutSmart
    , line
    , unAnnotateS
    )
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Pretty (renderIO)
import qualified Dhall
    ( Decoder(Decoder, expected, extract)
    , Encoder(Encoder, declared, embed)
    , Extractor
    , lazyText
    , natural
    , strictText
    , typeError
    )
import qualified Dhall.Core as Dhall
    ( Expr
        ( App
        , Field
        , List
        , ListLit
        , None
        , Optional
        , Record
        , RecordLit
        , Some
        , Text
        , TextLit
        , Union
        )
    , Import(Import, importHashed)
    , ImportHashed(ImportHashed, hash, importType)
    , ImportType
    , judgmentallyEqual
    , throws
    )
import qualified Dhall.Map as Dhall (Map)
import qualified Dhall.Map as Dhall.Map (delete, lookup, singleton)
import qualified Dhall.Pretty as Dhall
    ( Ann
    , CharacterSet
    , annToAnsiStyle
    , layoutOpts
    , prettyCharacterSet
    )
import qualified Dhall.Src as Dhall (Src)
import qualified System.Console.Terminal.Size as Terminal
    ( Window(Window, width)
    , hSize
    )

import CommandWrapper.Core.Config.ColourOutput (ColourOutput, shouldUseColours)


-- {{{ ToDhall Combinators ----------------------------------------------------

inputString :: Dhall.Encoder String
inputString = Dhall.Encoder
    { declared = Dhall.Text
    , embed = Dhall.TextLit . fromString
    }

inputList :: Dhall.Encoder a -> Dhall.Encoder [a]
inputList Dhall.Encoder{..} = Dhall.Encoder
    { declared = declared'
    , embed = Dhall.ListLit (Just declared') . fromList . fmap embed
    }
  where
    declared' = Dhall.List `Dhall.App` declared

inputMaybe :: Dhall.Encoder a -> Dhall.Encoder (Maybe a)
inputMaybe Dhall.Encoder{..} = Dhall.Encoder
    { declared = Dhall.Optional `Dhall.App` declared
    , embed = maybe (Dhall.None `Dhall.App` declared) (Dhall.Some . embed)
    }

-- }}} ToDhall Combinators ----------------------------------------------------
-- {{{ FromDhall Combinators --------------------------------------------------

newtype UnionType a =
    UnionType (Dhall.Map Text (Either a (Dhall.Decoder a)))

instance Functor UnionType where
    fmap :: forall a b. (a -> b) -> UnionType a -> UnionType b
    fmap f =
        coerce
            @( Dhall.Map Text (Either a (Dhall.Decoder a))
            -> Dhall.Map Text (Either b (Dhall.Decoder b))
            )
            (fmap (bimap f (fmap f)))

instance Semigroup (UnionType a) where
    (<>) = coerce ((<>) @(Dhall.Map Text (Either a (Dhall.Decoder a))))

instance Monoid (UnionType a) where
    mempty = coerce (mempty :: Dhall.Map Text (Either a (Dhall.Decoder a)))

-- | Parse a single constructor of a union.
constructor :: Text -> Dhall.Decoder a -> UnionType a
constructor key valueType =
    UnionType (Dhall.Map.singleton key (Right valueType))

-- | Parse a single constructor of a union.
constructor0 :: Text -> a -> UnionType a
constructor0 key value = UnionType (Dhall.Map.singleton key (Left value))

-- | Run a 'UnionType' parser to build a 'Dhall.Type' parser.
union :: forall a. UnionType a -> Dhall.Decoder a
union (UnionType constructors) = Dhall.Decoder
    { extract  = extractF
    , expected = Dhall.Union expect
    }
  where
    expect =
        either (const Nothing) (notEmptyRecord . Dhall.expected)
            <$> constructors

    unexpectedConstructor
        :: Dhall.Expr Dhall.Src Void
        -> Dhall.Extractor Dhall.Src Void a
    unexpectedConstructor = Dhall.typeError (Dhall.Union expect)

    extractF
        :: Dhall.Expr Dhall.Src Void
        -> Dhall.Extractor Dhall.Src Void a
    extractF e0 = fromMaybe (unexpectedConstructor e0) do
        (field, e1, rest) <- extractUnionConstructor e0

        t <- Dhall.Map.lookup field constructors
        guard
            ( Dhall.Union rest
                `Dhall.judgmentallyEqual`
                    Dhall.Union (Dhall.Map.delete field expect)
            )
        either (Just . pure) (Just . (`Dhall.extract` e1)) t

extractUnionConstructor
    :: Dhall.Expr s a
    -> Maybe (Text, Dhall.Expr s a, Dhall.Map Text (Maybe (Dhall.Expr s a)))
extractUnionConstructor = \case
    Dhall.App (Dhall.Field (Dhall.Union kts) fld) e ->
        pure (fld, e, Dhall.Map.delete fld kts)

    Dhall.Field (Dhall.Union kts) fld ->
        pure (fld, Dhall.RecordLit mempty, Dhall.Map.delete fld kts)

    _ ->
        empty

notEmptyRecord :: Dhall.Expr s a -> Maybe (Dhall.Expr s a)
notEmptyRecord = \case
    Dhall.Record m | null m -> empty
    e                       -> pure e

interpretWord :: Dhall.Decoder Word
interpretWord = fromIntegral <$> Dhall.natural

interpretLazyByteString :: Dhall.Decoder Lazy.ByteString
interpretLazyByteString = Lazy.Text.encodeUtf8 <$> Dhall.lazyText

interpretStrictByteString :: Dhall.Decoder Strict.ByteString
interpretStrictByteString = Strict.Text.encodeUtf8 <$> Dhall.strictText

-- }}} FromDhall Combinators --------------------------------------------------
-- {{{ I/O --------------------------------------------------------------------

-- | Print haskell value as a Dhall expression.
hPut
    :: ColourOutput
    -> Dhall.CharacterSet
    -> Handle
    -> Dhall.Encoder a
    -> a
    -> IO ()
hPut colour charset handle Dhall.Encoder{embed = toExpr} =
    hPutExpr colour charset handle . toExpr

-- | Print dhall expression.
hPutExpr
    :: Pretty a
    => ColourOutput
    -> Dhall.CharacterSet
    -> Handle
    -> Dhall.Expr Dhall.Src a
    -> IO ()
hPutExpr colour charset handle expr =
    hPutDoc colour handle (Dhall.prettyCharacterSet charset expr <> Pretty.line)

-- | Print Dhall-style 'Pretty.Doc' to specified handle.  Respects terminal
-- size and all.
hPutDoc :: ColourOutput -> Handle -> Pretty.Doc Dhall.Ann -> IO ()
hPutDoc colourOutput h doc = do
    layoutOpts <- Terminal.hSize h <&> \case
        Nothing ->
            Dhall.layoutOpts

        Just Terminal.Window{width} ->
            Pretty.LayoutOptions (Pretty.AvailablePerLine width 1.0)

    useColours <- shouldUseColours h colourOutput

    let stream = Pretty.layoutSmart layoutOpts doc

    Pretty.renderIO h
        if useColours
            then Dhall.annToAnsiStyle <$> stream
            else Pretty.unAnnotateS stream

-- }}} I/O --------------------------------------------------------------------
-- {{{ Secure Imports ---------------------------------------------------------

-- | A call to `assertSecureImports` failed because there was at least one
-- import import that wasn't protected by a hash.
data UnsecureImportResolutionDisabled = UnsecureImportResolutionDisabled
  deriving anyclass (Exception)

instance Show UnsecureImportResolutionDisabled where
    show _ = "\nUnsecure import resolution is disabled"

-- | Assert than all imports adhere to security policy.
assertSecureImports
    :: MonadIO io
    => (Dhall.ImportType -> Bool)
    -- ^ Security policy for individual 'ImportType's.  When this function
    -- returns 'True' it means that for that import type we require a Hash to
    -- be provided.
    -> Dhall.Expr Dhall.Src Dhall.Import
    -> io (Dhall.Expr Dhall.Src Dhall.Import)
assertSecureImports hashRequredFor expression = Dhall.throws
    $ for expression \i@Dhall.Import{importHashed = Dhall.ImportHashed{..}} ->
        if
          | Nothing <- hash, not (hashRequredFor importType) ->
                Left UnsecureImportResolutionDisabled
          | otherwise ->
                pure i

-- {{{ Secure Imports ---------------------------------------------------------
