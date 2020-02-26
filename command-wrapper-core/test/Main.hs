-- |
-- Module:      Main
-- Description: Tests for Command Wrapper Core library.
-- Copyright:   (c) 2019-2020 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions; POSIX.
--
-- Tests for Command Wrapper Core library.
module Main
    ( main
    )
  where

--import Data.CallStack (HasCallStack)
import Test.Tasty (TestTree, defaultMain, testGroup)
--import Test.Tasty.HUnit ((@?=), testCase, assertFailure)


main :: IO ()
main = defaultMain (testGroup "CommandWrapper.Core.Tests" tests)

tests :: [TestTree]
tests =
    [
    ]
