{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module:      Main
-- Description: CommandWrapper subcommand for changing directory by selecting
--              one from preselected list
-- Copyright:   (c) 2018 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions; POSIX.
--
-- CommandWrapper subcommand for changing directory by selecting one from
-- preselected list.
--
-- It is implemented as external command so that it can be completely
-- overridden if needed.
module Main (main)
  where

import Control.Applicative ((<|>))
import Control.Exception (onException)
import Data.Functor (void)
import GHC.Generics (Generic)
import System.Exit (die)

import Data.Text (Text)
import qualified Data.Text as Text (unpack)
import qualified Dhall (Interpret, auto, inputFile)
import System.Posix.Process (executeFile)
import Turtle
    ( cd
    , echo
    , fromText
    , inproc
    , liftIO
    , lineToText
    , options
    , select
    , sh
    , unsafeTextToLine
    )

import qualified CommandWrapper.Environment as Environment


-- TODO:
--
--   * Tmux support.
--   * Support for glob patterns in configuration? Would be useful for
--     something like `~/Devel/*`

data Config = Config
    { directories :: [Text]
    , menuTool :: Text
    , shell :: Text
    }
  deriving (Generic, Show)

instance Dhall.Interpret Config

main :: IO ()
main = do
    (_wrapperName, configFile) <- Environment.parseEnvIO (die . show) $ do
        void (Environment.askVar "COMMAND_WRAPPER_EXE")
            <|> failInvalidCommandWrapperEnvironment

        (,) <$> Environment.askVar "COMMAND_WRAPPER_NAME"
            <*> Environment.askVar "COMMAND_WRAPPER_CONFIG"

    options description (pure ())
    Config{..} <- Dhall.inputFile Dhall.auto configFile

    sh $ do
        line <- inproc menuTool []
            $ select (unsafeTextToLine <$> directories)

        echo ("+ : cd " <> line)
        cd (fromText $ lineToText line)
        echo ("+ : " <> unsafeTextToLine shell)
        liftIO
            $ executeFile (Text.unpack shell) False [] Nothing
                `onException` die "Error: Can not execute."
  where
    description =
        "Change directory by selecting one from preselected list"

    failInvalidCommandWrapperEnvironment =
        fail "This command must be executed as part of some command-wrapper environment"
