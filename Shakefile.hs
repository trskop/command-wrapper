#!/usr/bin/env stack
{- stack script
    --resolver lts-15.0
    --package directory
    --package executable-path
    --package shake
    --package time
    --
-}

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
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
-- Copyright:   (c) 2018-2020 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Install Command Wrapper, its documentation, and required tools.
module Main (main)
  where

import Data.Functor ((<&>))
import System.Exit (die)

import Data.Time.Clock.POSIX (getCurrentTime)
import System.Directory
    ( XdgDirectory(XdgConfig, XdgData)
    , getHomeDirectory
    , getXdgDirectory
    , setCurrentDirectory
    , createDirectoryIfMissing
    )
import System.Environment.Executable (ScriptPath(..), getScriptPath)

import Development.Shake
--import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Classes (Binary, Hashable, NFData)


newtype ThisGitRepo = ThisGitRepo ()
  deriving newtype (Binary, Eq, Hashable, NFData, Show)

type instance RuleResult ThisGitRepo = String

thisGitRepo :: FilePath -> ThisGitRepo -> Action String
thisGitRepo directory ThisGitRepo{} = do
    Stdout status <- cmd "git status -s --"
        [ "stack.yaml"
        , "Shakefile.hs"
        , "app-cd/"
        , "app-exec/"
        , "app-skel/"
        , "app/"
        , "bash/"
        , "dhall/"
        , "man/"
        , "package.yaml"
        , "src/"
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
            pure (takeDirectory executable)

        RunGHC script ->
            pure (takeDirectory script)

        Interactive ->
            die "Interactive mode not supported; call shakeMain directly."

    home <- getHomeDirectory
    configDir <- getXdgDirectory XdgConfig ""
    dataDir <- getXdgDirectory XdgData ""
    let localDir = home </> ".local"

    setCurrentDirectory projectRoot
    shakeMain Directories{..} shakeOptions

data ManDirs = ManDirs
    { man1Dir :: FilePath
    , man7Dir :: FilePath
    }

mkManDirs :: FilePath -> ManDirs
mkManDirs dir = ManDirs
    { man1Dir = dir </> "man1"
    , man7Dir = dir </> "man7"
    }

shakeMain :: Directories -> ShakeOptions -> IO ()
shakeMain Directories{..} opts = shakeArgs opts do
    let localLibDir = localDir </> "lib"
        libexecDir = localLibDir </> "command-wrapper"

        -- Standard `man` command should be able to pick this up.  Try
        -- `manpath` after installation to be sure.
        manDir = dataDir </> "man"

        staticOut = projectRoot </> "out"
        staticOutDist = projectRoot </> "out" </> "command-wrapper"
        staticOutDistShare = staticOutDist </> "share"
        staticLibexecDir = staticOutDist </> "libexec" </> "command-wrapper"
        staticManDir = staticOutDistShare </> "man"
        staticDocDir = staticOutDistShare </> "doc" </> "command-wrapper"

        version = "0.1.0.0-rc2"
        staticTarball =
            staticOut </> "command-wrapper-" <> version <.> "tar.xz"

        manPages :: ManDirs -> [FilePath]
        manPages ManDirs{..} =
            [ man1Dir </> "command-wrapper.1.gz"
            , man1Dir </> "command-wrapper-cd.1.gz"
            , man1Dir </> "command-wrapper-completion.1.gz"
            , man1Dir </> "command-wrapper-config.1.gz"
            , man1Dir </> "command-wrapper-exec.1.gz"
            , man1Dir </> "command-wrapper-help.1.gz"
            , man1Dir </> "command-wrapper-skel.1.gz"
            , man1Dir </> "command-wrapper-version.1.gz"
            , man7Dir </> "command-wrapper-bash-library.7.gz"
            , man7Dir </> "command-wrapper-subcommand-protocol.7.gz"
            ]

        manPagePatterns :: ManDirs -> [String]
        manPagePatterns ManDirs{..} =
            [ man1Dir </> "*.1.gz"
            , man7Dir </> "*.7.gz"
            ]

        manHtml :: ManDirs -> [FilePath]
        manHtml dirs = manPages dirs <&> (-<.> "html")

        manHtmlPatterns :: ManDirs -> [FilePath]
        manHtmlPatterns ManDirs{..} =
            [ man1Dir </> "*.1.html"
            , man7Dir </> "*.7.html"
            ]

        binaries :: FilePath -> [FilePath]
        binaries dir =
            [ dir </> "command-wrapper"
            , dir </> "command-wrapper-cd"
            , dir </> "command-wrapper-exec"
            , dir </> "command-wrapper-skel"
            ]

    want $ mconcat
        [ binaries libexecDir
        , ["man"]
        ]

    "man" ~>
        need (manPages (mkManDirs manDir))

    "man-html" ~>
        need (manHtml (ManDirs staticDocDir staticDocDir))

    "static" ~>
        need [staticTarball, staticTarball <.> "sha256sum"]

    staticTarball %> \out -> do
        need $ mconcat
            [ manPages (mkManDirs staticManDir)
            , manHtml (ManDirs staticDocDir staticDocDir)
            , binaries staticLibexecDir
            ]

        cmd_ "tar"
            [ "-C", takeDirectory out
            , "--owner=root:0"
            , "--group=root:0"
            , "--mtime=1970-01-01T00:00:00Z"
            , "--mode=a-w"
            , "-cJf", out
            , takeFileName staticOutDist
            ]

    staticTarball <.> "sha256sum" %> \out -> do
        let src = dropExtension out
            dir = takeDirectory out
        need [src]
        cmd_ [Cwd dir, FileStdout out] "sha256sum" [takeFileName src]

    hasThisRepoChanged <- addOracle (thisGitRepo projectRoot)
    binaries libexecDir &%> \outs -> do
        _ <- hasThisRepoChanged (ThisGitRepo ())

        let dst = takeDirectory (head outs)
        liftIO (createDirectoryIfMissing True dst)

        cmd_ "stack" ["--local-bin-path=" <> dst] "install"

    binaries staticLibexecDir &%> \outs -> do
        _ <- hasThisRepoChanged (ThisGitRepo ())

        let dst = takeDirectory (head outs)
        liftIO (createDirectoryIfMissing True dst)

        -- Without closing stdin stack would hang.
        cmd_ [Stdin ""] "stack"
            [ "--local-bin-path=" <> dst
            , "--resolver=lts-14.27" -- GHC 8.6.5 to be consistent with image
            , "--docker"
            , "--docker-image=ghc-musl:v4-libgmp-ghc865-extended"
            , "install"
            , "--flag=command-wrapper:static"
            ]

    manPagePatterns (mkManDirs manDir) |%> \out -> do
        let tempOut = dropExtension out
            src = "man" </> takeFileName out -<.> "md"
            dst = takeDirectory out

        need [src]

        liftIO (createDirectoryIfMissing True dst)

        cmd_ "pandoc --standalone --to=man" ["--output=" <> tempOut, src]
        cmd_ "gzip --force -9" [tempOut]

    manPagePatterns (mkManDirs staticManDir) |%> \out -> do
        let tempOut = dropExtension out
            src = "man" </> takeFileName out -<.> "md"
            dst = takeDirectory out

        need [src]

        liftIO (createDirectoryIfMissing True dst)

        cmd_ "pandoc --standalone --to=man" ["--output=" <> tempOut, src]
        cmd_ "gzip --force -9" [tempOut]

    manHtmlPatterns (ManDirs staticDocDir staticDocDir) |%> \out -> do
        let src = "man" </> takeFileName out -<.> "md"
            dst = takeDirectory out

        need [src]

        liftIO (createDirectoryIfMissing True dst)

        cmd_ "pandoc --standalone --to=html" ["--output=" <> out, src]

-- vim:ft=haskell
