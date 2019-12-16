{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:      CommandWrapper.Internal.Subcommand.Completion
-- Description: Supported shell values and '--shell=SHELL' option.
-- Copyright:   (c) 2018-2019 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Supported 'Shell' values and @--shell=SHELL@ option.
module CommandWrapper.Options.Shell
    ( Shell(..)
    , parse

    , HasShell(..)
    , setShell

    , shellOption
    )
  where

import Data.Either (Either(Left, Right))
import Data.Eq (Eq)
import Data.Function (($), (.), const, id)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Monoid (Endo(Endo), (<>))
import Data.String (IsString)
import GHC.Generics (Generic)
import Text.Show (Show)

import qualified Data.CaseInsensitive as CI (mk)
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
  deriving stock (Generic, Show)
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

instance HasShell Shell where
    updateShell :: Endo Shell -> Endo Shell
    updateShell = id

setShell :: HasShell a => Shell -> Endo a
setShell = updateShell . Endo . const
