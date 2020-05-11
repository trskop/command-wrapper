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

import Control.Monad (unless)
import Data.Functor ((<&>))
import Data.Maybe (fromJust)
import System.Exit (ExitCode(..))
import System.IO (hClose, hGetContents, hPutStr)

import Control.DeepSeq (deepseq)
import Data.CallStack (HasCallStack)
import Test.Tasty (TestTree, defaultMain, localOption, testGroup)
import Test.Tasty.HUnit ((@?=), testCase, assertFailure)
import Test.Tasty.Options (parseValue)
import Test.Tasty.Program (CatchStderr, testProgram)
import System.Process (runInteractiveProcess, waitForProcess)

import CommandWrapper.Toolset.InternalSubcommand.Completion.Libraries
    ( DhallLibrary(..)
    , ImportOrContent(..)
    , showDhallLibrary
    )

main :: IO ()
main = defaultMain $ testGroup "CommandWrapper.Tests"
    [ testGroup "DhallLibraries" testDhallLibraries
    , catchStderr $ testGroup "BashLibrary"
        [ testBashSubcommandLibrary
        ]
    , testGroup "Subcommand"
        [ testGroup "Exec" testExecSubcommand
        ]
    ]
  where
    -- Library `tasty-program` hides constructor, so this is the only way I
    -- could come up with.
    catchStderr = localOption (fromJust (parseValue "True") :: CatchStderr)

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
    , [Content, Import] <&> testDhallLibrary PreludeV16_0_0
    , [Content, Import] <&> testDhallLibrary PreludeV15_0_0
    , [Content, Import] <&> testDhallLibrary PreludeV14_0_0
    , [Content, Import] <&> testDhallLibrary PreludeV13_0_0
    , [Content, Import] <&> testDhallLibrary PreludeV12_0_0
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
        PreludeV12_0_0     -> "v12.0.0"
        PreludeV13_0_0     -> "v13.0.0"
        PreludeV14_0_0     -> "v14.0.0"
        PreludeV15_0_0     -> "v15.0.0"
        PreludeV16_0_0     -> "v16.0.0"
        LatestPrelude      -> "latest (v16.0.0)"
        CommandWrapper     -> notTestable
        CommandWrapperExec -> notTestable

    actualHash = case lib of
        PreludeV12_0_0     -> v12_0_0
        PreludeV13_0_0     -> v13_0_0
        PreludeV14_0_0     -> v14_0_0
        PreludeV15_0_0     -> v15_0_0
        PreludeV16_0_0     -> v16_0_0
        LatestPrelude      -> v16_0_0
        CommandWrapper     -> notTestable
        CommandWrapperExec -> notTestable

    notTestable =  error ("Not designed to test this library " <> show lib)

    v12_0_0 =
        "sha256:aea6817682359ae1939f3a15926b84ad5763c24a3740103202d2eaaea4d01f4c"

    v13_0_0 =
        "sha256:4aa8581954f7734d09b7b21fddbf5d8df901a44b54b4ef26ea71db92de0b1a12"

    v14_0_0 =
        "sha256:c1b3fc613aabfb64a9e17f6c0d70fe82016a030beedd79851730993e9083fde2"

    v15_0_0 =
        "sha256:6b90326dc39ab738d7ed87b970ba675c496bed0194071b332840a87261649dcd"

    v16_0_0 =
        "sha256:7e2b87add393288298baabc73119601182d04630b9989bdb9ac0822dc0863b38"

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

testBashSubcommandLibrary :: TestTree
testBashSubcommandLibrary =
    testProgram "Subcommand" "bash" ["./test/lib-tests.bash", "./bash/lib.bash"]
        Nothing

testExecSubcommand :: [TestTree]
testExecSubcommand =
    [ testCase "Expression.MakeExecCommand" $ testExpression
        "  λ(verbosity : < Annoying | Normal | Silent | Verbose >)\
        \→ λ(colourOutput : < Always | Auto | Never >)\
        \→ λ(arguments : List Text)\
        \→ { arguments = arguments\
        \  , command = \"echo\"\
        \  , environment = [] : List { name : Text, value : Text }\
        \  , searchPath = True\
        \  , workingDirectory = None Text\
        \  }"
        ["Just", "some", "text"]
        "Just some text\n"
    , testCase "Expression.ExecCommand" $ testExpression
        "{ arguments = [ \"Just\", \"some\" ]\
        \, command = \"echo\"\
        \, environment = [] : List { name : Text, value : Text }\
        \, searchPath = True\
        \, workingDirectory = None Text\
        \}"
        ["text"]
        "Just some text\n"
    , testCase "Expression.Command" $ testExpression
        "{ arguments = [ \"Just\", \"some\" ]\
        \, command = \"echo\"\
        \}"
        ["text"]
        "Just some text\n"
    , testCase "Expression.ExecNamedCommand" $ testExpression
        "{ name = \"echo\"\
        \, description = None Text\
        \, command = \
        \      λ(verbosity : < Annoying | Normal | Silent | Verbose >)\
        \    → λ(colourOutput : < Always | Auto | Never >)\
        \    → λ(arguments : List Text)\
        \    → { arguments = arguments\
        \      , command = \"echo\"\
        \      , environment = [] : List { name : Text, value : Text }\
        \      , searchPath = True\
        \      , workingDirectory = None Text\
        \      }\
        \, completion = None\
        \    (   < Bash | Fish | Zsh >\
        \      → Natural\
        \      → List Text\
        \      → { arguments : List Text\
        \        , command : Text\
        \        , environment : List { name : Text, value : Text }\
        \        , searchPath : Bool, workingDirectory : Optional Text\
        \        }\
        \    )\
        \, notifyWhen = None < After : Natural | Always | Never | OnFailure >\
        \}"
        ["Just", "some", "text"]
        "Just some text\n"
    ]
  where
    testExpression expr arguments expectedOutput = do
        r <- callCommandWrapper
            (["exec", "--expression=" <> expr] <> arguments)
            ""
        r @?= expectedOutput
