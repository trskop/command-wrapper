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
    ( hPut
    , hPutExpr
    )
  where

import System.IO (Handle, hPutStrLn)

import Data.Text.Prettyprint.Doc (Pretty)
import qualified Dhall (InputType(InputType, embed))
import qualified Dhall.Core as Dhall (Expr)
import qualified Dhall.Pretty as Dhall
    ( CharacterSet
    , annToAnsiStyle
    , prettyCharacterSet
    )

import CommandWrapper.Message (hPutDoc, defaultLayoutOptions)
import CommandWrapper.Options.ColourOutput (ColourOutput)


hPut
    :: ColourOutput
    -> Dhall.CharacterSet
    -> Handle
    -> Dhall.InputType a
    -> a
    -> IO ()
hPut colour charset handle Dhall.InputType{embed = toExpr} =
    hPutExpr colour charset handle . toExpr

hPutExpr
    :: Pretty a
    => ColourOutput
    -> Dhall.CharacterSet
    -> Handle
    -> Dhall.Expr s a
    -> IO ()
hPutExpr colour charset handle expr = do
    hPutDoc defaultLayoutOptions Dhall.annToAnsiStyle colour handle
        $ Dhall.prettyCharacterSet charset expr
    hPutStrLn handle ""
