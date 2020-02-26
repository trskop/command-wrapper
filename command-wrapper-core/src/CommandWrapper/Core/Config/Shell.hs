{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:      CommandWrapper.Core.Config.Shell
-- Description: Supported shell values and '--shell=SHELL' option.
-- Copyright:   (c) 2018-2020 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Supported 'Shell' values and @--shell=SHELL@ option.
module CommandWrapper.Core.Config.Shell
    ( Shell(..)
    , parse

    , HasShell(..)
    , setShell

    , shellOption
    )
  where

import Data.Coerce (coerce)
import Data.Either (Either(Left, Right))
import Data.Eq (Eq)
import Data.Function (($), (.), const, id)
import Data.Functor.Identity (Identity(Identity))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Monoid (Endo(Endo), (<>))
import Data.String (IsString)
import GHC.Generics (Generic)
import Text.Show (Show)

import qualified Data.CaseInsensitive as CI (mk)
import Data.Generics.Product.Typed (HasType, typed)
import Dhall (FromDhall, ToDhall)
import qualified Options.Applicative as Options
    ( Parser
    , eitherReader
    , long
    , metavar
    , option
    )


-- | Enum of supported shells.
data Shell
    = Bash
    | Fish
    | Zsh
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromDhall, ToDhall)

parse :: (Eq s, IsString s) => s -> Maybe Shell
parse = \case
    "Bash" -> Just Bash
    "Fish" -> Just Fish
    "Zsh"  -> Just Zsh
    _      -> Nothing
    -- TODO: Use Generics instead.

-- | @--shell=SHELL@
shellOption :: HasShell a => Options.Parser (Endo a)
shellOption =
    Options.option parse' (Options.long "shell" <> Options.metavar "SHELL")
  where
    parse' = Options.eitherReader $ \s -> case parse (CI.mk s) of
        Just sh  -> Right (setShell sh)
        _        -> Left "Unrecognised shell name"

class HasShell a where
    updateShell :: Endo Shell -> Endo a

    default updateShell :: HasType Shell a => Endo Shell -> Endo a
    updateShell (Endo f) = Endo \a -> coerce (l (coerce . f) a)
      where
        l = typed :: (Shell -> Identity Shell) -> a -> Identity a

instance HasShell Shell where
    updateShell :: Endo Shell -> Endo Shell
    updateShell = id

setShell :: HasShell a => Shell -> Endo a
setShell = updateShell . Endo . const
