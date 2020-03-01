{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       Main
-- Description:  Top-level executable of Command Wrapper.
-- Copyright:    (c) 2014-2020 Peter Trsko
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  GHC specific language extensions.
--
-- Top-level executable of Command Wrapper, it's usually installed as
-- @${HOME}/.local/lib/command-wrapper/command-wrapper@.
module Main (main)
  where

import System.IO (IO)

import qualified CommandWrapper.Toolset.Main (main)
import qualified CommandWrapper.Toolset.Main.StaticConfig as StaticConfig


main :: IO ()
main =
    CommandWrapper.Toolset.Main.main StaticConfig.def
#if defined(STATIC_EXECUTABLE) || defined(NIX_EXECUTABLE)
        { StaticConfig.lookupSystemConfigDir =
            StaticConfig.doSystemConfigDirLookup
        }
#endif
