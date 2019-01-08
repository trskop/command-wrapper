-- |
-- Module:      CommandWrapper.Internal.Dhall
-- Description: TODO: Module synopsis
-- Copyright:   (c) 2018-2019 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- TODO: Module description.
module CommandWrapper.Internal.Dhall
--  (
--  )
  where

import GHC.Generics (Generic)
import System.IO (Handle, hPutStrLn)

import qualified Dhall (InputType(InputType, embed))
import qualified Dhall.Main as Dhall (Mode)
import qualified Dhall.Binary as Dhall (StandardVersion)
import qualified Dhall.Pretty as Dhall
    ( CharacterSet
    , annToAnsiStyle
    , prettyCharacterSet
    )
import qualified Dhall.JSON

import qualified CommandWrapper.Config.Global as Global
import CommandWrapper.Message (hPutDoc, defaultLayoutOptions)
import CommandWrapper.Options.ColourOutput (ColourOutput)


data DhallOptions = DhallOptions
    { explain :: Bool
    , protocolVersion :: Dhall.StandardVersion
    }

data BashMode

data DhallMode a
    = DhallMode Dhall.Mode            DhallOptions a
    | DhallBash BashMode              DhallOptions a
    | DhallJson Dhall.JSON.Conversion DhallOptions a
    | DhallText                       DhallOptions a
  deriving (Functor, Generic)

dhall :: DhallMode Global.Config -> IO ()
dhall = undefined

hPut
    :: ColourOutput
    -> Dhall.CharacterSet
    -> Handle
    -> Dhall.InputType a
    -> a
    -> IO ()
hPut colour charset handle Dhall.InputType{embed = toExpr} a = do
    hPutDoc defaultLayoutOptions Dhall.annToAnsiStyle colour handle
        $ Dhall.prettyCharacterSet charset (toExpr a)
    hPutStrLn handle ""
