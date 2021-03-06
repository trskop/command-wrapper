-- |
-- Module:      $Header$
-- Description: Data type that represents when desktop notifications should be
--              raised.
-- Copyright:   (c) 2019-2020 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Data type that represents when desktop notifications should be raised.
module CommandWrapper.Toolset.Config.NotifyWhen
    ( NotifyWhen(..)
    , interpretNotifyWhen
    )
  where

import Data.Eq (Eq)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Monoid (mconcat)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import Text.Show (Show)

import Dhall (FromDhall)
import qualified Dhall
    ( FromDhall(autoWith)
    , InputNormalizer
    , InterpretOptions(InterpretOptions, constructorModifier)
    , Decoder
    , defaultInterpretOptions
    , natural
    )

import qualified CommandWrapper.Core.Dhall as Dhall
    ( constructor
    , constructor0
    , union
    )


data NotifyWhen
    = Never
    -- ^ Never notify.
    | Always
    -- ^ Always notify.
    | OnFailure
    -- ^ Notify only when something failed.
    | After Natural
    -- ^ Notify only if action took more than the specified number of seconds.
  deriving stock (Eq, Generic, Show)

interpretNotifyWhen :: Dhall.InterpretOptions -> Dhall.Decoder NotifyWhen
interpretNotifyWhen Dhall.InterpretOptions{constructorModifier = f} =
    Dhall.union $ mconcat
        [ Dhall.constructor0 (f "Never")      Never
        , Dhall.constructor0 (f "Always")     Always
        , Dhall.constructor0 (f "OnFailure")  OnFailure
        , Dhall.constructor  (f "After")     (After <$> Dhall.natural)
        ]

instance FromDhall NotifyWhen where
    autoWith :: Dhall.InputNormalizer -> Dhall.Decoder NotifyWhen
    autoWith _ = interpretNotifyWhen Dhall.defaultInterpretOptions

--inputNotifyWhen :: Dhall.Encoder NotifyWhen
