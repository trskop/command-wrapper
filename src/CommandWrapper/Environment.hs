{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:      CommandWrapper.Environment
-- Description: Application environment and environment variables.
-- Copyright:   (c) 2018 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Application environment and environment variables.
module CommandWrapper.Environment
    (
    -- * Environment Variables
      module CommandWrapper.Environment.Variable

    -- * Environment Builder
    , module CommandWrapper.Environment.Builder

    -- * Environment Parser
    , module CommandWrapper.Environment.Parser

    -- * Application Params
    --
    -- | Subcommand parameters passed via Subcommand Protocol.
    , module CommandWrapper.Environment.Params

    -- * Application Names
    --
    -- | Names and paths under which Command Wrapper toolset is known.
    , module CommandWrapper.Environment.AppNames
    )
  where

import CommandWrapper.Environment.AppNames
import CommandWrapper.Environment.Builder
import CommandWrapper.Environment.Params
import CommandWrapper.Environment.Parser
import CommandWrapper.Environment.Variable
