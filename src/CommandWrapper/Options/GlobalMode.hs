{-# LANGUAGE InstanceSigs #-}
-- |
-- Module:      CommandWrapper.Options.GlobalMode
-- Description: Top-level application mode
-- Copyright:   (c) 2018-2019 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Top-level application mode.  It provides the ability for us to handle help
-- without interference from option parsing library.
module CommandWrapper.Options.GlobalMode
    ( GlobalMode(..)
    , runGlobalMode
    )
  where

import Data.Monoid (Endo(Endo))

import Control.Comonad (Comonad(extend, extract))


-- | This allows global options parser to change application mode, but in a
-- very limited way.
data GlobalMode a
    = HelpMode a
    -- ^ Switch to internal help command.
    | PreserveMode a
  deriving (Functor)

instance Comonad GlobalMode where
    extract :: GlobalMode a -> a
    extract = \case
        HelpMode a -> a
        PreserveMode a -> a

    extend :: (GlobalMode a -> b) -> GlobalMode a -> GlobalMode b
    extend f mode = case mode of
        HelpMode _ -> HelpMode b
        PreserveMode _ -> PreserveMode b
      where
        b = f mode

runGlobalMode
    :: Functor mode
    => (forall b. Endo b -> Endo (mode b))
    -- ^ Switch to \"help\" mode.
    -> GlobalMode (Endo a)
    -> Endo (mode a)
runGlobalMode helpMode = \case
    HelpMode f            -> helpMode f
    PreserveMode (Endo f) -> Endo (fmap f)
