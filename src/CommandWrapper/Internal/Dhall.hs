-- |
-- Module:      CommandWrapper.Internal.Dhall
-- Description: Dhall utilities.
-- Copyright:   (c) 2018-2019 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Dhall utilities.
module CommandWrapper.Internal.Dhall
    (
    -- * Interpret Combinators
      interpretWord
    , interpretLazyByteString
    , interpretStrictByteString

    -- ** Unions
    , UnionType(..)
    , constructor
    , constructor0
    , union

    -- * Inject Combinators
    , inputString
    , inputList
    , inputMaybe

    -- * I/O
    , hPut
    , hPutExpr
    , hPutDoc
    )
  where

import Prelude hiding ((<>))

import Control.Applicative (empty)
import Control.Monad (guard)
import Data.Bifunctor (bimap)
import Data.Coerce (coerce)
import Data.Functor ((<&>))
import Data.Semigroup ((<>))
import Data.String (fromString)
import GHC.Exts (IsList(fromList))
import System.IO (Handle)

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
    ( InputType(InputType, declared, embed)
    , Type(Type, expected, extract)
    , lazyText
    , natural
    , strictText
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
        , UnionLit
        )
    , judgmentallyEqual
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
import qualified System.Console.Terminal.Size as Terminal
    ( Window(Window, width)
    , hSize
    )

import CommandWrapper.Options.ColourOutput (ColourOutput, shouldUseColours)


-- {{{ Inject Combinators -----------------------------------------------------

inputString :: Dhall.InputType String
inputString = Dhall.InputType
    { declared = Dhall.Text
    , embed = Dhall.TextLit . fromString
    }

inputList :: Dhall.InputType a -> Dhall.InputType [a]
inputList Dhall.InputType{..} = Dhall.InputType
    { declared = Dhall.List `Dhall.App` declared
    , embed = Dhall.ListLit (Just declared) . fromList . fmap embed
    }

inputMaybe :: Dhall.InputType a -> Dhall.InputType (Maybe a)
inputMaybe Dhall.InputType{..} = Dhall.InputType
    { declared = Dhall.Optional `Dhall.App` declared
    , embed = maybe (Dhall.None `Dhall.App` declared) (Dhall.Some . embed)
    }

-- }}} Inject Combinators -----------------------------------------------------
-- {{{ Interpret Combinators --------------------------------------------------

newtype UnionType a =
    UnionType (Dhall.Map Text (Either a (Dhall.Type a)))

instance Functor UnionType where
    fmap :: forall a b. (a -> b) -> UnionType a -> UnionType b
    fmap f =
        coerce
            @( Dhall.Map Text (Either a (Dhall.Type a))
            -> Dhall.Map Text (Either b (Dhall.Type b))
            )
            (fmap (bimap f (fmap f)))

instance Semigroup (UnionType a) where
    (<>) = coerce ((<>) @(Dhall.Map Text (Either a (Dhall.Type a))))

instance Monoid (UnionType a) where
    mempty = coerce (mempty :: Dhall.Map Text (Either a (Dhall.Type a)))
    mappend = (<>)

-- | Parse a single constructor of a union.
constructor :: Text -> Dhall.Type a -> UnionType a
constructor key valueType =
    UnionType (Dhall.Map.singleton key (Right valueType))

-- | Parse a single constructor of a union.
constructor0 :: Text -> a -> UnionType a
constructor0 key value = UnionType (Dhall.Map.singleton key (Left value))

-- | Run a 'UnionType' parser to build a 'Dhall.Type' parser.
union :: forall a. UnionType a -> Dhall.Type a
union (UnionType constructors) = Dhall.Type
    { extract  = extractF
    , expected = Dhall.Union expect
    }
  where
    expect =
        either (const Nothing) (notEmptyRecord . Dhall.expected)
            <$> constructors

    extractF e0 = do
        (field, e1, rest) <- extractUnionConstructor e0

        t <- Dhall.Map.lookup field constructors
        guard
            ( Dhall.Union rest
                `Dhall.judgmentallyEqual`
                    Dhall.Union (Dhall.Map.delete field expect)
            )
        either Just (`Dhall.extract` e1) t

extractUnionConstructor
    :: Dhall.Expr s a
    -> Maybe (Text, Dhall.Expr s a, Dhall.Map Text (Maybe (Dhall.Expr s a)))
extractUnionConstructor = \case
    Dhall.UnionLit fld e rest ->
        pure (fld, e, rest)

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

interpretWord :: Dhall.Type Word
interpretWord = fromIntegral <$> Dhall.natural

interpretLazyByteString :: Dhall.Type Lazy.ByteString
interpretLazyByteString = Lazy.Text.encodeUtf8 <$> Dhall.lazyText

interpretStrictByteString :: Dhall.Type Strict.ByteString
interpretStrictByteString = Strict.Text.encodeUtf8 <$> Dhall.strictText

-- }}} Interpret Combinators --------------------------------------------------
-- {{{ I/O --------------------------------------------------------------------

-- | Print haskell value as a Dhall expression.
hPut
    :: ColourOutput
    -> Dhall.CharacterSet
    -> Handle
    -> Dhall.InputType a
    -> a
    -> IO ()
hPut colour charset handle Dhall.InputType{embed = toExpr} =
    hPutExpr colour charset handle . toExpr

-- | Print dhall expression.
hPutExpr
    :: Pretty a
    => ColourOutput
    -> Dhall.CharacterSet
    -> Handle
    -> Dhall.Expr s a
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
