#!/usr/bin/env stack
{- stack script
    --resolver lts-13.17
    --package directory
    --package executable-path
    --package shake
    --package time
    --
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wall #-}

-- |
-- Module:      Main
-- Description: Install Command Wrapper, its documentation, and required tools.
-- Copyright:   (c) 2018-2019 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Install Command Wrapper, its documentation, and required tools.
module Main (main)
  where

import Control.Monad (unless)
import Data.List (isPrefixOf)
import System.Exit (die)

import Data.Time.Clock.POSIX (getCurrentTime)
import System.Directory
    ( XdgDirectory(XdgConfig, XdgData)
    , getHomeDirectory
    , getXdgDirectory
    , setCurrentDirectory
    )
import System.Environment.Executable (ScriptPath(..), getScriptPath)

import Development.Shake
--import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Classes (Binary, Hashable, NFData)


newtype ThisGitRepo = ThisGitRepo ()
  deriving (Binary, Eq, Hashable, NFData, Show)

type instance RuleResult ThisGitRepo = String

thisGitRepo :: FilePath -> ThisGitRepo -> Action String
thisGitRepo directory ThisGitRepo{} = do
    Stdout status <- cmd "git status -s --"
        [ "stack.yaml"
        , "package.yaml"
        , "app/"
        , "app-cd/"
        , "app-exec/"
        , "app-skel/"
        , "src/"
        , "man/"
        ]
    if null @[] @Char status
        then do
            Stdout hash <- cmd "git -C" [directory] "show-ref -s origin/HEAD"
            pure hash
        else
            ("Workspace dirty " <>) . show <$> liftIO getCurrentTime

data Directories = Directories
    { home :: FilePath
    , projectRoot :: FilePath
    , configDir :: FilePath
    , dataDir :: FilePath
    , localDir :: FilePath
    }

main :: IO ()
main = do
    projectRoot <- getScriptPath >>= \case
        Executable executable ->
            pure $ takeDirectory executable

        RunGHC script ->
            pure $ takeDirectory script

        Interactive ->
            die "Interactive mode not supported; call shakeMain directly."

    home <- getHomeDirectory
    configDir <- getXdgDirectory XdgConfig ""
    dataDir <- getXdgDirectory XdgData ""
    let localDir = home </> ".local"

    setCurrentDirectory projectRoot
    shakeMain Directories{..} shakeOptions

shakeMain :: Directories -> ShakeOptions -> IO ()
shakeMain Directories{..} opts = shakeArgs opts $ do
    let localLibDir = localDir </> "lib"
        localBinDir = localDir </> "bin"

        commandWrapperLibexecDir = localLibDir </> "command-wrapper"

        commandWrapperBin = commandWrapperLibexecDir </> "command-wrapper"
        cdBin = commandWrapperLibexecDir </> "command-wrapper-cd"
        execBin = commandWrapperLibexecDir </> "command-wrapper-exec"
        skelBin = commandWrapperLibexecDir </> "command-wrapper-skel"

        -- Standard `man` command should be able to pick this up.  Try
        -- `manpath` after installation to be sure.
        manDir = dataDir </> "man"
        man1Dir = manDir </> "man1"
        man7Dir = manDir </> "man7"

    want
        [ commandWrapperBin
        , cdBin
        , execBin
        , skelBin

        , localBinDir </> "dhall"
        , localBinDir </> "dhall-to-bash"
        , localBinDir </> "dhall-to-json"  -- Implies "dhall-to-yaml".
        , localBinDir </> "dhall-to-text"

        , man1Dir </> "command-wrapper.1.gz"
        , man1Dir </> "command-wrapper-cd.1.gz"
        , man1Dir </> "command-wrapper-completion.1.gz"
        , man1Dir </> "command-wrapper-config.1.gz"
        , man1Dir </> "command-wrapper-exec.1.gz"
        , man1Dir </> "command-wrapper-help.1.gz"
        , man1Dir </> "command-wrapper-skel.1.gz"
        , man1Dir </> "command-wrapper-version.1.gz"
        , man7Dir </> "command-wrapper-subcommand-protocol.7.gz"
        ]

    hasThisRepoChanged <- addOracle (thisGitRepo projectRoot)
    [commandWrapperBin, cdBin, execBin, skelBin] &%> \outs -> do
        _ <- hasThisRepoChanged (ThisGitRepo ())

        let dst = takeDirectory (head outs)
        targetExists <- doesDirectoryExist dst
        unless targetExists
            $ cmd_ "mkdir -p" [dst]

        cmd_ "stack" ["--local-bin-path=" <> dst] "install"

    (localBinDir </> "dhall*") %> \out -> do
        -- TODO: Oracle that checks version.
        let outFile = takeFileName out
            package =
                if "dhall-to-" `isPrefixOf` outFile
                    then "dhall" <> drop 8 outFile
                    else outFile

        cmd_ "stack install" [package]

    [man1Dir </> "*.1.gz", man7Dir </> "*.7.gz"] |%> \out -> do
        let tempOut = dropExtension out
            src = "man" </> dropExtension (takeFileName out) <.> "md"
            dst = takeDirectory out

        need [src]

        targetExists <- doesDirectoryExist dst
        unless targetExists
            $ cmd_ "mkdir -p" [dst]

        cmd_ "pandoc --standalone --to=man" ["--output=" <> tempOut, src]
        cmd_ "gzip --force -9" [tempOut]

-- vim:ft=haskell
