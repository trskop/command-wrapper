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
    , applyAliasCompletion
    )
  where

import Prelude ((+))

import Data.Eq ((==))
import qualified Data.List as List (find, genericLength)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Monoid ((<>))
import Data.String (String)
import Data.Word (Word)
import GHC.Generics (Generic)
import Text.Show (Show)

import qualified Dhall


data Alias = Alias
    { alias :: String
    , description :: Maybe String
    , command :: String
    , arguments :: [String]
    }
  deriving stock (Generic, Show)
  deriving anyclass (Dhall.Interpret)

applyAlias :: [Alias] -> String -> [String] -> (String, [String])
applyAlias aliases subcommand arguments =
    let (cmd, args, _) = applyAliasCompletion aliases subcommand arguments 0
     in (cmd, args)

-- | Same as 'applyAlias', but also offsets an index pointing to the argument
-- list when the argument list changes.
applyAliasCompletion
    :: [Alias]
    -> String
    -> [String]
    -> Word
    -> (String, [String], Word)
applyAliasCompletion aliases subcommand arguments index =
    case List.find (\Alias{alias} -> alias == subcommand) aliases of
        Nothing ->
            (subcommand, arguments, index)

        Just Alias{command, arguments = args} ->
            (command, args <> arguments, index + List.genericLength args)
