{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
-- |
-- Module:      CommandWrapper.Internal.Dhall
-- Description: TODO: Module synopsis
-- Copyright:   (c) 2018 Peter Trško
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

import Dhall.Main as Dhall (Mode)
import Dhall.Binary as Dhall (StandardVersion)
import qualified Dhall.JSON

import qualified CommandWrapper.Config.Global as Global


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
