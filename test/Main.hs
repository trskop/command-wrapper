-- |
-- Module:      Main
-- Description: Tests for Command Wrapper.
-- Copyright:   (c) 2019 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions; POSIX.
--
-- Tests for Command Wrapper.
module Main
--  (
--  )
  where

import Control.Monad (unless)
import System.Exit (ExitCode(..))
import System.IO (hClose, hGetContents, hPutStr)

import Control.DeepSeq (deepseq)
import Data.CallStack (HasCallStack)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit ((@?=), testCase, assertFailure)
import System.Process (runInteractiveProcess, waitForProcess)


main :: IO ()
main = defaultMain $ testGroup "CommandWrapper.Tests"
    [ testCase "dhall/CommandWrapper lib is consistent" do
        let pkgFile = "./dhall/CommandWrapper/package.dhall"
        actualHash <- dhallHash ["--no-cache", "--input=" <> pkgFile] ""
        expectedHash <- dhallHash ["--input=" <> pkgFile] ""

        actualHash @?= expectedHash

    , testCase "dhall/CommandWrapper lib is consistent with completion" do
        let pkgFile = "./dhall/CommandWrapper/package.dhall"
        lib <- callCommandWrapper
            ["completion", "--library", "--dhall=command-wrapper"] ""
        actualHash <- dhallHash [] lib
        expectedHash <- dhallHash ["--input=" <> pkgFile] ""

        actualHash @?= expectedHash

    , testCase "dhall/CommandWrapper lib is consistent with completion import" do
        let pkgFile = "./dhall/CommandWrapper/package.dhall"
        libImport <- callCommandWrapper
            ["completion", "--library", "--dhall=command-wrapper", "--import"]
            ""
        actualHash <- dhallHash ["--expression=" <> libImport] ""
        expectedHash <- dhallHash ["--input=" <> pkgFile] ""

        actualHash @?= expectedHash

    , testCase "dhall/Exec lib is consistent" do
        let pkgFile = "./dhall/Exec/package.dhall"
        actualHash <- dhallHash ["--no-cache", "--input=" <> pkgFile] ""
        expectedHash <- dhallHash ["--input=" <> pkgFile] ""

        actualHash @?= expectedHash

    , testCase "dhall/Exec lib is consistent with completion" do
        let pkgFile = "./dhall/Exec/package.dhall"
        lib <- callCommandWrapper ["completion", "--library", "--dhall=exec"] ""
        actualHash <- dhallHash [] lib
        expectedHash <- dhallHash ["--input=" <> pkgFile] ""

        actualHash @?= expectedHash

    , testCase "dhall/Exec lib is consistent with completion import" do
        let pkgFile = "./dhall/Exec/package.dhall"
        libImport <- callCommandWrapper
            ["completion", "--library", "--dhall=exec", "--import"] ""
        actualHash <- dhallHash ["--expression=" <> libImport] ""
        expectedHash <- dhallHash ["--input=" <> pkgFile] ""

        actualHash @?= expectedHash

    , testCase "dhall/Prelude v12.0.0 lib is consistent" do
        let actualHash =
                "sha256:aea6817682359ae1939f3a15926b84ad5763c24a3740103202d2eaaea4d01f4c"
        lib <- callCommandWrapper
            ["completion", "--library", "--dhall=prelude-v12.0.0"] ""
        expectedHash <- dhallHash [] lib

        (actualHash <> "\n") @?= expectedHash

    , testCase "dhall/Prelude v12.0.0 lib is consistent" do
        let actualHash =
                "sha256:aea6817682359ae1939f3a15926b84ad5763c24a3740103202d2eaaea4d01f4c"
        lib <- callCommandWrapper
            ["completion", "--library", "--dhall=prelude-v12.0.0", "--import"]
            ""
        expectedHash <- dhallHash [] lib

        (actualHash <> "\n") @?= expectedHash

    , testCase "dhall/Prelude v11.1.0 lib is consistent" do
        let actualHash =
                "sha256:99462c205117931c0919f155a6046aec140c70fb8876d208c7c77027ab19c2fa"
        lib <- callCommandWrapper
            ["completion", "--library", "--dhall=prelude-v11.1.0"] ""
        expectedHash <- dhallHash [] lib

        (actualHash <> "\n") @?= expectedHash

    , testCase "dhall/Prelude v11.1.0 lib is consistent" do
        let actualHash =
                "sha256:99462c205117931c0919f155a6046aec140c70fb8876d208c7c77027ab19c2fa"
        lib <- callCommandWrapper
            ["completion", "--library", "--dhall=prelude-v11.1.0", "--import"]
            ""
        expectedHash <- dhallHash [] lib

        (actualHash <> "\n") @?= expectedHash

    ]

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
    unless (null inputValue) do
        hPutStr input inputValue
        hClose input
    errout <- hGetContents err
    stdout <- hGetContents out
    deepseq (stdout, errout) (waitForProcess h) >>= \case
        ExitSuccess ->
            pure stdout

        ExitFailure status ->
            assertFailure $ unwords
                [ "Command Wrapper terminated with exit status"
                , show status
                , "when called with"
                , show fullArgs
                , if null errout
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
dhallHash args = callCommandWrapper (["config", "--dhall-hash"] <> args)
