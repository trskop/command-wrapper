-- |
-- Module:      Main
-- Description: Tests for Command Wrapper.
-- Copyright:   (c) 2019-2020 Peter Trško
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
import Data.Functor ((<&>))
import System.Exit (ExitCode(..))
import System.IO (hClose, hGetContents, hPutStr)

import Control.DeepSeq (deepseq)
import Data.CallStack (HasCallStack)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit ((@?=), testCase, assertFailure)
import System.Process (runInteractiveProcess, waitForProcess)

import CommandWrapper.Internal.Subcommand.Completion.Libraries
    ( DhallLibrary(..)
    , ImportOrContent(..)
    , showDhallLibrary
    )

main :: IO ()
main = defaultMain $ testGroup "CommandWrapper.Tests"
    [ testGroup "DhallLibraries" testDhallLibraries
    ]

testDhallLibraries :: [TestTree]
testDhallLibraries = concat
    [ [ testCase "CommandWrapper lib is hashed correctly" do
            let pkgFile = "./dhall/CommandWrapper/package.dhall"
            actualHash <- dhallHash ["--no-cache", "--input=" <> pkgFile] ""
            expectedHash <- dhallHash ["--input=" <> pkgFile] ""

            actualHash @?= expectedHash
      ]

    , [Content, Import] <&> \importOrContent -> do
        let testName =
                "CommandWrapper lib " <> show importOrContent
                <> " is consistent"
        testCase testName  do
            let pkgFile = "./dhall/CommandWrapper/package.dhall"
            lib <- callCommandWrapper
                [ "completion"
                , "--library"
                , dhallLibOption CommandWrapper
                , case importOrContent of
                    Content -> "--content"
                    Import -> "--import"
                ] ""
            actualHash <- dhallHash [] lib
            expectedHash <- dhallHash ["--input=" <> pkgFile] ""

            actualHash @?= expectedHash

    , [ testCase "Exec lib is hashed correctly" do
            let pkgFile = "./dhall/Exec/package.dhall"
            actualHash <- dhallHash ["--no-cache", "--input=" <> pkgFile] ""
            expectedHash <- dhallHash ["--input=" <> pkgFile] ""

            actualHash @?= expectedHash
      ]

    , [Content, Import] <&> \importOrContent -> do
        let testName = "Exec lib " <> show importOrContent <> " is consistent"
        testCase testName do
            let pkgFile = "./dhall/Exec/package.dhall"
            lib <- callCommandWrapper
                [ "completion"
                , "--library"
                , dhallLibOption CommandWrapperExec
                , case importOrContent of
                    Content -> "--content"
                    Import -> "--import"
                ] ""
            actualHash <- dhallHash [] lib
            expectedHash <- dhallHash ["--input=" <> pkgFile] ""

            actualHash @?= expectedHash

    , [Content, Import] <&> testDhallLibrary LatestPrelude
    , [Content, Import] <&> testDhallLibrary PreludeV13_0_0
    , [Content, Import] <&> testDhallLibrary PreludeV12_0_0
    , [Content, Import] <&> testDhallLibrary PreludeV11_1_0
    ]

dhallLibOption :: DhallLibrary -> String
dhallLibOption lib = "--dhall=" <> showDhallLibrary lib

testDhallLibrary :: DhallLibrary -> ImportOrContent -> TestTree
testDhallLibrary lib importOrContent =
    testCase (libName <> " is consistent") do
        expr <- callCommandWrapper
            [ "completion"
            , "--library"
            , dhallLibOption lib
            , case importOrContent of
                Import -> "--import"
                Content -> "--content"
            ]
            ""
        expectedHash <- dhallHash [] expr
        (actualHash <> "\n") @?= expectedHash
  where
    libName = (\v -> unwords ["Prelude", v, show importOrContent]) case lib of
        PreludeV11_1_0     -> "v11.1.0"
        PreludeV12_0_0     -> "v12.0.0"
        PreludeV13_0_0     -> "v13.0.0"
        LatestPrelude      -> "latest (v13.0.0)"
        CommandWrapper     -> notTestable
        CommandWrapperExec -> notTestable

    actualHash = case lib of
        PreludeV11_1_0     -> v11_1_0
        PreludeV12_0_0     -> v12_0_0
        PreludeV13_0_0     -> v13_0_0
        LatestPrelude      -> v13_0_0
        CommandWrapper     -> notTestable
        CommandWrapperExec -> notTestable

    notTestable =  error ("Not designed to test this library " <> show lib)

    v11_1_0 =
        "sha256:99462c205117931c0919f155a6046aec140c70fb8876d208c7c77027ab19c2fa"

    v12_0_0 =
        "sha256:aea6817682359ae1939f3a15926b84ad5763c24a3740103202d2eaaea4d01f4c"

    v13_0_0 =
        "sha256:4aa8581954f7734d09b7b21fddbf5d8df901a44b54b4ef26ea71db92de0b1a12"

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
