{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:      CommandWrapper.Config.NotifyWhen
-- Description: Data type that represents when desktop notifications should be
--              raised.
-- Copyright:   (c) 2019 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Data type that represents when desktop notifications should be raised.
module CommandWrapper.Config.NotifyWhen
--  (
--  )
  where

import Data.Eq (Eq)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Monoid (mconcat)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import Text.Show (Show)

import Data.Text (Text)
import Dhall (FromDhall)
import qualified Dhall
    ( FromDhall(autoWith)
    , InterpretOptions(InterpretOptions, constructorModifier)
    , Decoder
    , natural
    )
import qualified CommandWrapper.Internal.Dhall as Dhall
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

interpretNotifyWhen :: (Text -> Text) -> Dhall.Decoder NotifyWhen
interpretNotifyWhen f = Dhall.union $ mconcat
    [ Dhall.constructor0 (f "Never")      Never
    , Dhall.constructor0 (f "Always")     Always
    , Dhall.constructor0 (f "OnFailure")  OnFailure
    , Dhall.constructor  (f "After")     (After <$> Dhall.natural)
    ]

instance FromDhall NotifyWhen where
    autoWith :: Dhall.InterpretOptions -> Dhall.Decoder NotifyWhen
    autoWith Dhall.InterpretOptions{constructorModifier} =
        interpretNotifyWhen constructorModifier

--inputNotifyWhen :: Dhall.Encoder NotifyWhen
