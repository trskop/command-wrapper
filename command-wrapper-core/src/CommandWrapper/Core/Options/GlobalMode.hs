-- |
-- Module:      $Header$
-- Description: Top-level application mode
-- Copyright:   (c) 2018-2020 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Top-level application mode.  It provides the ability for us to handle help
-- without interference from option parsing library.
module CommandWrapper.Core.Options.GlobalMode
    ( GlobalMode(..)
    , switchGlobalMode
    , runGlobalMode
    )
  where

import Data.Function ((.))
import Data.Functor (Functor, fmap)

import Data.Monoid (Endo(Endo))

import Control.Comonad (Comonad(extend, extract))


-- | This allows global options parser to change application mode, but in a
-- very limited way.
data GlobalMode a
    = HelpMode a
    -- ^ Switch to internal @help@ command.
    | VersionMode a
    -- ^ Switch to internal @version@ command.
    | PreserveMode a
  deriving stock (Functor)

instance Comonad GlobalMode where
    extract :: GlobalMode a -> a
    extract = \case
        HelpMode a -> a
        VersionMode a -> a
        PreserveMode a -> a

    extend :: (GlobalMode a -> b) -> GlobalMode a -> GlobalMode b
    extend f mode = case mode of
        HelpMode _ -> HelpMode b
        VersionMode _ -> VersionMode b
        PreserveMode _ -> PreserveMode b
      where
        b = f mode

switchGlobalMode
    :: (forall a. a -> GlobalMode a)
    -> GlobalMode config
    -> GlobalMode config
switchGlobalMode f = f . extract

runGlobalMode
    :: Functor mode
    => (forall b. Endo b -> Endo (mode b))
    -- ^ Switch to \"help\" mode.
    -> (forall b. Endo b -> Endo (mode b))
    -- ^ Switch to \"version\" mode.
    -> GlobalMode (Endo a)
    -> Endo (mode a)
runGlobalMode helpMode versionMode = \case
    HelpMode f            -> helpMode f
    VersionMode f         -> versionMode f
    PreserveMode (Endo f) -> Endo (fmap f)
