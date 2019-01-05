{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:      CommandWrapper.Options.Alias
-- Description: Subcommand aliases; their definition and evaluation
-- Copyright:   (c) 2018-2019 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Subcommand aliases; their definition and evaluation.
module CommandWrapper.Options.Alias
    ( Alias(..)
    , applyAlias
    )
  where

import Data.Eq ((==))
import qualified Data.List as List (find)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Monoid ((<>))
import Data.String (String)
import GHC.Generics (Generic)
import Text.Show (Show)

import qualified Dhall


data Alias = Alias
    { alias :: String
    , command :: String
    , arguments :: [String]
    }
  deriving stock (Generic, Show)
  deriving anyclass (Dhall.Interpret)

applyAlias :: [Alias] -> String -> [String] -> (String, [String])
applyAlias aliases subcommand arguments =
    case List.find (\Alias{alias} -> alias == subcommand) aliases of
        Nothing ->
            (subcommand, arguments)

        Just Alias{command, arguments = args} ->
            (command, args <> arguments)
