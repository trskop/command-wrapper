-- |
-- Module:      Tests.Subcommand.Exec
-- Description: Tests for Command Wrapper's "exec" subcommand
-- Copyright:   (c) 2020 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Tests for Command Wrapper's @exec@ subcommand.
module Tests.Subcommand.Exec
    ( tests
    )
  where

import Prelude

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)

import Utilities (callCommandWrapper)


tests :: [TestTree]
tests =
    [ testExecExpression
    ]

testExecExpression :: TestTree
testExecExpression = testGroup "Expression"
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
