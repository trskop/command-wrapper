{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:      CommandWrapper.Options.Alias
-- Description: TODO: Module synopsis
-- Copyright:   (c) 2018 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- TODO: Module description.
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

import qualified Dhall


data Alias = Alias
    { alias :: String
    , command :: String
    , arguments :: [String]
    }
  deriving (Generic)

instance Dhall.Interpret Alias

applyAlias :: [Alias] -> String -> [String] -> (String, [String])
applyAlias aliases subcommand arguments =
    case List.find (\Alias{alias} -> alias == subcommand) aliases of
        Nothing ->
            (subcommand, arguments)

        Just Alias{command, arguments = args} ->
            (command, args <> arguments)
