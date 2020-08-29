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
import Data.Foldable (for_)
import Data.Maybe (fromJust)
import System.Exit (ExitCode(..))
import System.IO (hClose, hGetContents, hPutStr)

import Control.DeepSeq (deepseq)
import Data.CallStack (HasCallStack)
import Test.Tasty (TestTree, defaultMain, localOption, testGroup)
import Test.Tasty.HUnit ((@?=), testCase, assertEqual, assertFailure)
import Test.Tasty.Options (parseValue)
import Test.Tasty.Program (CatchStderr, testProgram)
import System.FilePath ((-<.>))
import System.FilePath.Glob (glob)
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
        LatestPrelude      -> "latest (v17.1.0)"
        CommandWrapper     -> notTestable
        CommandWrapperExec -> notTestable

    actualHash = case lib of
        PreludeV17_0_0     -> v17_0_0
        PreludeV17_1_0     -> v17_1_0
        LatestPrelude      -> v17_1_0
        CommandWrapper     -> notTestable
        CommandWrapperExec -> notTestable

    notTestable =  error ("Not designed to test this library " <> show lib)

    v17_0_0 =
        "sha256:10db3c919c25e9046833df897a8ffe2701dc390fa0893d958c3430524be5a43e"

    -- Yes, it's the same hash as v17_0_0
    v17_1_0 =
        "sha256:10db3c919c25e9046833df897a8ffe2701dc390fa0893d958c3430524be5a43e"

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
dhallHash args = fmap (takeWhile (/= '\n'))
    .  callCommandWrapper (["config", "--dhall-hash"] <> args)

dhallText
    :: HasCallStack
    => [String]
    -> String
    -> IO String
dhallText args = callCommandWrapper (["config", "--dhall-text"] <> args)

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
