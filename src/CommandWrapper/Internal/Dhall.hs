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

    -- * Inject Combinators
      inputString
    , inputList
    , inputMaybe

    -- * I/O
    , hPut
    , hPutExpr
    , hPutDoc
    )
  where

import Data.Functor ((<&>))
import Data.String (fromString)
import GHC.Exts (IsList(fromList))
import System.IO (Handle)

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
import qualified Dhall (InputType(InputType, declared, embed))
import qualified Dhall.Core as Dhall
    ( Expr
        ( App
        , List
        , ListLit
        , None
        , Optional
        , Some
        , Text
        , TextLit
        )
    )
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


-- {{{ Combinators ------------------------------------------------------------

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

-- }}} Combinators ------------------------------------------------------------
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
