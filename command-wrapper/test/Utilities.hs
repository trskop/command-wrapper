-- |
-- Module:      Utilities
-- Description: Helper functions and utilities for Command Wrapper tests
-- Copyright:   (c) 2019-2020 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions; POSIX.
--
-- Helper functions and utilities for Command Wrapper tests.
module Utilities
    ( callCommandWrapper
    , dhallHash
    , dhallText
    )
  where

import Control.Applicative (pure)
import Control.Monad ((>>=), unless)
import Data.Eq ((/=))
import Data.Function (($), (.))
import Data.Functor (fmap)
import qualified Data.List as List (null, takeWhile, unwords)
import Data.Maybe (Maybe(Nothing))
import Data.Semigroup ((<>))
import Data.String (String)
import System.Exit (ExitCode(..))
import System.IO (IO, hClose, hGetContents, hPutStr)
import Text.Show (show)

import Control.DeepSeq (deepseq)
import Data.CallStack (HasCallStack)
import System.Process (runInteractiveProcess, waitForProcess)
import Test.Tasty.HUnit (assertFailure)

callCommandWrapper
    :: HasCallStack
    => [String]
    -- ^ Command line arguments.
    -> String
    -- ^ Standard input.
    -> IO String
    -- ^ Standard output.
callCommandWrapper args inputValue = do
    (input, out, err, h) <- callCommandWrapper'
    unless (List.null inputValue) do
        hPutStr input inputValue
        hClose input
    errout <- hGetContents err
    stdout <- hGetContents out
    deepseq (stdout, errout) (waitForProcess h) >>= \case
        ExitSuccess ->
            pure stdout

        ExitFailure status ->
            assertFailure $ List.unwords
                [ "Command Wrapper terminated with exit status"
                , show status
                , "when called with"
                , show fullArgs
                , if List.null errout
                    then "and without printing anything to stderr."
                    else "and while printing following to stderr:\n" <> errout
                ]
  where
    fullArgs = ("--no-aliases" : args)

    callCommandWrapper' =
        runInteractiveProcess "command-wrapper" fullArgs Nothing Nothing

dhallHash
    :: HasCallStack
    => [String]
    -> String
    -> IO String
dhallHash args = fmap (List.takeWhile (/= '\n'))
    .  callCommandWrapper (["config", "--dhall-hash"] <> args)

dhallText
    :: HasCallStack
    => [String]
    -> String
    -> IO String
dhallText args = callCommandWrapper (["config", "--dhall-text"] <> args)
