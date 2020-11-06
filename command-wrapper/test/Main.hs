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
module Main (main)
  where

import Prelude

import Data.Functor ((<&>))
import Data.Foldable (for_)
import Data.Maybe (fromJust)

import Test.Tasty (TestTree, defaultMain, localOption, testGroup)
import Test.Tasty.HUnit ((@?=), testCase, assertEqual)
import Test.Tasty.Options (parseValue)
import Test.Tasty.Program (CatchStderr, testProgram)
import System.FilePath ((-<.>))
import System.FilePath.Glob (glob)

import CommandWrapper.Toolset.InternalSubcommand.Completion.Libraries
    ( DhallLibrary(..)
    , ImportOrContent(..)
    , parseDhallLibrary
    , showDhallLibrary
    )

import Utilities (callCommandWrapper, dhallHash, dhallText)
import qualified Tests.Subcommand.Config (tests)
import qualified Tests.Subcommand.Exec (tests)


main :: IO ()
main = defaultMain $ testGroup "CommandWrapper"
    [ testGroup "DhallLibraries" testDhallLibraries
    , catchStderr $ testGroup "BashLibrary"
        [ testBashSubcommandLibrary
        ]
    , testGroup "Subcommand"
        [ testGroup "config" Tests.Subcommand.Config.tests
        , testGroup "exec" Tests.Subcommand.Exec.tests
        ]
    , testDhallLibraryEnum
    ]
  where
    -- Library `tasty-program` hides constructor, so this is the only way I
    -- could come up with.
    catchStderr = localOption (fromJust (parseValue "True") :: CatchStderr)

testDhallLibraries :: [TestTree]
testDhallLibraries = concat
    [   -- Checks that import hashes are consistent with what is actually
        -- present in the repository.
      [ testCase "package.dhall files are consistent" do
            files <- glob "dhall/**/package.dhall"
            for_ files \packageDhall -> do
                actualHash <- dhallHash ["--no-cache", "--input=" <> packageDhall] ""
                expectedHash <- dhallHash ["--input=" <> packageDhall] ""

                assertEqual packageDhall expectedHash actualHash

        -- Every "completion-script.dhall" has corresponding
        -- "completion-script.hash.dhall" file, both are then imported by
        -- "completion.dhall". This way we can check that the script that we
        -- are referring to in "completion.dhall" is the same one as is present
        -- in the repository.
      , testCase "completion-script.hash.dhall files are consistent" do
            files <- glob "completion-script.dhall"
            for_ files \completionScript -> do
                actualHash <- dhallHash
                    ["--no-cache", "--input=" <> completionScript] ""
                expectedHash <- dhallText
                    ["--input=" <> (completionScript -<.> "hash.dhall")] ""

                assertEqual completionScript expectedHash actualHash

      , testCase "CommandWrapper lib is hashed correctly" do
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
    , [Content, Import] <&> testDhallLibrary PreludeV17_0_0
    , [Content, Import] <&> testDhallLibrary PreludeV17_1_0
    , [Content, Import] <&> testDhallLibrary PreludeV18_0_0
    , [Content, Import] <&> testDhallLibrary PreludeV19_0_0
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
        actualHash @?= expectedHash
  where
    libName = (\v -> unwords ["Prelude", v, show importOrContent]) case lib of
        PreludeV17_0_0     -> "v17.0.0"
        PreludeV17_1_0     -> "v17.1.0"
        PreludeV18_0_0     -> "v18.0.0"
        PreludeV19_0_0     -> "v19.0.0"
        LatestPrelude      -> "latest (v19.0.0)"
        CommandWrapper     -> notTestable
        CommandWrapperExec -> notTestable

    actualHash = case lib of
        PreludeV17_0_0     -> v17_0_0
        PreludeV17_1_0     -> v17_1_0
        PreludeV18_0_0     -> v18_0_0
        PreludeV19_0_0     -> v19_0_0
        LatestPrelude      -> v19_0_0
        CommandWrapper     -> notTestable
        CommandWrapperExec -> notTestable

    notTestable =  error ("Not designed to test this library " <> show lib)

    v17_0_0 =
        "sha256:10db3c919c25e9046833df897a8ffe2701dc390fa0893d958c3430524be5a43e"

    -- Yes, it's the same hash as v17_0_0
    v17_1_0 =
        "sha256:10db3c919c25e9046833df897a8ffe2701dc390fa0893d958c3430524be5a43e"

    v18_0_0 =
        "sha256:2086c6a2e7ce8ee371858036ab60d24d045969defadb4bc9d87639921ec2e028"

    v19_0_0 =
        "sha256:eb693342eb769f782174157eba9b5924cf8ac6793897fc36a31ccbd6f56dafe2"

testBashSubcommandLibrary :: TestTree
testBashSubcommandLibrary =
    testProgram "Subcommand" "bash" ["./test/lib-tests.bash", "./bash/lib.bash"]
        Nothing

testDhallLibraryEnum :: TestTree
testDhallLibraryEnum = testGroup "DhallLibrary"
    [ testGroup "(parseDhallLibrary . showDhallLibrary) === Just" do
        [minBound..maxBound] <&> \lib -> testCase (show lib) do
            parseDhallLibrary (showDhallLibrary @String lib) @?= Just lib
    ]
