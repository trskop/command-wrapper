-- |
-- Module:      $Header$
-- Description: Subcommand aliases; their definition and evaluation
-- Copyright:   (c) 2018-2020 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Subcommand aliases; their definition and evaluation.
module CommandWrapper.Core.Config.Alias
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

import Dhall (FromDhall)


-- | Represents an alias of a subcommand invocation.
data Alias = Alias
    { alias :: String
    -- ^ Name under which this alias is known and can be invoked.
    , description :: Maybe String
    -- ^ Short description of what the alias does.
    , command :: String
    -- ^ Subcommand that should be invoked.
    , arguments :: [String]
    -- ^ Arguments passed to the subcommand ('command').  Anything passed
    -- directly on the command line (when invoked via alias) will be appended
    -- to this list.
    }
  deriving stock (Generic, Show)
  deriving anyclass (FromDhall)

-- | Resolve aliases into subcommand invocations.  If passed subcommand name
-- is not an alias then it is left untouched.
applyAlias
    :: [Alias]
    -> String
    -- ^ Subcommand or alias name.
    -> [String]
    -- ^ Arguments passed to toolset when subcommand or alias was invoked.
    -> (String, [String])
    -- ^ Subcommand to be actually invoked with specified arguments.
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
