{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
-- |
-- Module:      Main
-- Description: CommandWrapper subcommand for generating subcommand skeletons
-- Copyright:   (c) 2018 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- CommandWrapper subcommand for generating subcommand skeleton files from
-- language specific templates.
--
-- It is implemented as external command so that it can be completely
-- overridden if needed.
module Main (main)
  where

import Control.Applicative ((<|>))
import Data.Functor (void)
import Data.String (fromString)
import GHC.Generics (Generic)
import System.Exit (die)

import Data.Text (Text)
import qualified Dhall (Interpret, auto, inputFile)
import qualified Data.Text as Text (unpack)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import qualified Turtle

import qualified CommandWrapper.Environment as Environment


data Config = Config
    { targetFile :: FilePath
    , templates :: Templates
    }
  deriving (Generic, Show)

instance Dhall.Interpret Config

data Templates = Templates
    { haskell :: String
    , bash :: String
    }
  deriving (Generic, Show)

instance Dhall.Interpret Templates

main :: IO ()
main = do
    (wrapperName, configFile) <- Environment.parseEnvIO (die . show) $ do
        void (Environment.askVar "COMMAND_WRAPPER_EXE")
            <|> failInvalidCommandWrapperEnvironment

        (,) <$> Environment.askVar "COMMAND_WRAPPER_NAME"
            <*> Environment.askVar "COMMAND_WRAPPER_CONFIG"

    mkConfig <- Dhall.inputFile Dhall.auto configFile
    subcommandName <- Turtle.options description parseOptions

    let subcommand = wrapperName <> "-" <> subcommandName
        Config{targetFile, templates} =
            mkConfig defaultSubcommandDescription
                (fromString @Text wrapperName)
                (fromString @Text subcommand)

    -- This may be the first command of that specific toolset.
    createDirectoryIfMissing True (takeDirectory targetFile)
    writeFile targetFile (haskell templates)
    -- TODO: Make the file executable.
    putStrLn ("Successfully created '" <> targetFile <> "'")
  where
    description =
        "Generate subcommand skeleton for specific command-wrapper environment"

    -- TODO: Option for this.
    defaultSubcommandDescription :: Text
    defaultSubcommandDescription = "TODO: Describe me!"

    failInvalidCommandWrapperEnvironment =
        fail "This command must be executed as part of some command-wrapper environment"

parseOptions :: Turtle.Parser String
parseOptions =
    Turtle.arg parseSubcommandName "SUBCOMMAND" "Name of the new subcommand"
    -- TODO: Option for selecting target language.

parseSubcommandName :: Text -> Maybe String
parseSubcommandName = \case
    "" -> Nothing
    t -> Just (Text.unpack t)
        -- TODO: Check that subcommand name consists of only reasonable ASCII
        --       characters. Any other reasonable restrictions on subcommand
        --       name?
