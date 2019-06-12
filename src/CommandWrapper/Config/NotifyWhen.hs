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

import GHC.Generics (Generic)
import Numeric.Natural (Natural)

import qualified Dhall (Type, natural)
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

interpretNotifyWhen :: Dhall.Type NotifyWhen
interpretNotifyWhen = Dhall.union $ mconcat
    [ Dhall.constructor0 "Never"      Never
    , Dhall.constructor0 "Always"     Always
    , Dhall.constructor0 "OnFailure"  OnFailure
    , Dhall.constructor  "After"     (After <$> Dhall.natural)
    ]

--inputNotifyWhen :: Dhall.InputType NotifyWhen
