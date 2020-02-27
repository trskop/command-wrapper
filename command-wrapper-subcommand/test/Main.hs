-- |
-- Module:      Main
-- Description: Tests for Command Wrapper Subcommand library.
-- Copyright:   (c) 2019-2020 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions; POSIX.
--
-- Tests for Command Wrapper Subcommand library.
module Main
    ( main
    )
  where

import System.IO (IO)

--import Data.CallStack (HasCallStack)
import Test.Tasty (TestTree, defaultMain, testGroup)
--import Test.Tasty.HUnit ((@?=), testCase, assertFailure)


main :: IO ()
main = defaultMain (testGroup "CommandWrapper.Subcommand.Tests" tests)

tests :: [TestTree]
tests =
    [
    ]
