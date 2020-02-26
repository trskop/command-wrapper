{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:      CommandWrapper.Core.Environment
-- Description: Application environment and environment variables.
-- Copyright:   (c) 2018-2020 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Application environment and environment variables.
module CommandWrapper.Core.Environment
    (
    -- * Environment Variables
      module CommandWrapper.Core.Environment.Variable

    -- * Environment Builder
    , module CommandWrapper.Core.Environment.Builder

    -- * Environment Parser
    , module CommandWrapper.Core.Environment.Parser

    -- * Application Params
    --
    -- | Subcommand parameters passed via Subcommand Protocol.
    --
    -- See also `subcommand-protocol(7)` manual page.
    , module CommandWrapper.Core.Environment.Params

    -- * Application Names
    --
    -- | Names and paths under which Command Wrapper toolset is known.
    --
    -- See also `command-wrapper(1)` and `subcommand-protocol(7)` manual pages.
    , module CommandWrapper.Core.Environment.AppNames
    )
  where

import CommandWrapper.Core.Environment.AppNames
import CommandWrapper.Core.Environment.Builder
import CommandWrapper.Core.Environment.Params
import CommandWrapper.Core.Environment.Parser
import CommandWrapper.Core.Environment.Variable
