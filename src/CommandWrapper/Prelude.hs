{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:      CommandWrapper.Prelude
-- Description: Give subcommands everything they need to seamlessly integrate.
-- Copyright:   (c) 2018-2020 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions; POSIX.
--
-- Give subcommands everything they need to seamlessly integrate.
module CommandWrapper.Prelude
    {-# DEPRECATED "Use CommandWrapper.Subcommand.Prelude instead." #-}
    (
      module CommandWrapper.Subcommand.Prelude
    )
  where

import CommandWrapper.Subcommand.Prelude
