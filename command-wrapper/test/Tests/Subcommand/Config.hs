-- |
-- Module:      Tests.Subcommand.Config
-- Description: Tests for Command Wrapper's "config --init" functionality
-- Copyright:   (c) 2020 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions; POSIX.
--
-- Tests for Command Wrapper's @config --init@ functionality.
module Tests.Subcommand.Config
    ( tests
    )
  where

import Prelude

import Data.Foldable (traverse_)
import Data.Functor ((<$))

import Data.CallStack (HasCallStack)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ({-(@?=),-} testCase{-, assertEqual-}, assertFailure)
import System.FilePath ((</>))
--import System.FilePath.Glob (glob)
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive)
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory)

import Utilities (callCommandWrapper)


tests :: [TestTree]
tests =
    [ testConfigInit
    ]

testConfigInit :: TestTree
testConfigInit = testGroup "--init"
    [ {-testCase "command-wrapper" do
        (dirs, temp) <- prepareDirectoryStructure
        _out <- configInit dirs Nothing

        assertFailure temp
        --removeDirectoryRecursive temp
    ,-}
      testCase "command-wrapper+toolset" do
        (dirs, temp) <- prepareDirectoryStructure
        _ <- configInit dirs Nothing
        _ <- configInit dirs (Just "some-toolset")

        assertFailure temp
        --removeDirectoryRecursive temp
--  , testCase "toolset" do
--      (dirs, temp) <- prepareDirectoryStructure
--      (dir, _) <- configInit dirs "some-toolset"

--      assertFailure temp
--      --removeDirectoryRecursive temp
    ]

data ConfigInitDirs = ConfigInitDirs
    { bin :: FilePath
    , config :: FilePath
    , libexec :: FilePath
    , man :: FilePath
    }

prepareDirectoryStructure :: HasCallStack => IO (ConfigInitDirs, FilePath)
prepareDirectoryStructure = do
    root <- getCanonicalTemporaryDirectory
    temp <- createTempDirectory root "command-wrapper-tests"
    let dirs@ConfigInitDirs{..} = ConfigInitDirs
            { bin = temp </> "bin"
            , config = temp </> "config"
            , libexec = temp </> "libexec"
            , man = temp </> "man"
            }
    traverse_ (createDirectoryIfMissing True) [bin, config, libexec, man]
    pure (dirs, temp)

configInit :: HasCallStack => ConfigInitDirs -> Maybe String -> IO String
configInit ConfigInitDirs{..} toolsetName = callCommandWrapper
    (   [ "config"
        , "--init"
        , "--bin-directory=" <> bin
        , "--config-directory=" <> config
        , "--libexec-directory=" <> libexec
        , "--man-directory=" <> man
        ]
    <>  maybe [] (\n -> ["--toolset=" <> n]) toolsetName
    )
    "" -- Empty standard input
