{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
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

import Control.Applicative (optional)
import Control.Monad (when)
import Data.Maybe (fromMaybe)
import Data.String (fromString)
import GHC.Generics (Generic)
import System.Exit (die)

import Data.CaseInsensitive as CI (mk)
import Data.Text (Text)
import qualified Data.Text.IO as Text (writeFile)
import qualified Dhall (Interpret, auto, inputFile)
import qualified Data.Text as Text (unpack)
import System.Directory
    ( createDirectoryIfMissing
    , getPermissions
    , setPermissions
    )
import qualified System.Directory as Directory (executable)
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
    { haskell :: Template
    , bash :: Template
    }
  deriving (Generic, Show)

instance Dhall.Interpret Templates

data Template = Template
    { targetFile :: Maybe FilePath
    , template :: Text
    , executable :: Bool
    }
  deriving (Generic, Show)

instance Dhall.Interpret Template

main :: IO ()
main = do
    Environment.Params{name = wrapperName, config = configFile} <- parseEnv
    mkConfig <- Dhall.inputFile Dhall.auto configFile
    (subcommandName, possiblyLanguage) <- Turtle.options description parseOptions

    let subcommand = wrapperName <> "-" <> subcommandName
        Config{targetFile, templates = Templates{..}} =
            mkConfig defaultSubcommandDescription
                (fromString @Text wrapperName)
                (fromString @Text subcommand)

    createdFile <- generateSkeleton targetFile $ case possiblyLanguage of
        Nothing -> haskell
        Just Haskell -> haskell
        Just Bash -> bash

    putStrLn ("Successfully created '" <> createdFile <> "'")
  where
    description =
        "Generate subcommand skeleton for specific command-wrapper environment"

    -- TODO: Option for this.
    defaultSubcommandDescription :: Text
    defaultSubcommandDescription = "TODO: Describe me!"

generateSkeleton :: FilePath -> Template -> IO FilePath
generateSkeleton defaultTarget Template{executable, targetFile, template} = do
    let destFile = fromMaybe defaultTarget targetFile

    -- This may be the first command of that specific toolset.
    createDirectoryIfMissing True (takeDirectory destFile)
    Text.writeFile destFile template
    when executable $ do
        perms <- getPermissions destFile
        setPermissions destFile (perms{Directory.executable = True})

    pure destFile

parseEnv :: IO Environment.Params
parseEnv = Environment.parseEnvIO (die . show) Environment.askParams

parseOptions :: Turtle.Parser (String, Maybe Language)
parseOptions =
    (,) <$> Turtle.arg parseSubcommandName "SUBCOMMAND"
                "Name of the new subcommand"
        <*> optional
                ( Turtle.opt parseLanguage "language" 'l'
                    "Choose programming language of subcommand skeleton"
                )

parseSubcommandName :: Text -> Maybe String
parseSubcommandName = \case
    "" -> Nothing
    t -> Just (Text.unpack t)
        -- TODO: Check that subcommand name consists of only reasonable ASCII
        --       characters. Any other reasonable restrictions on subcommand
        --       name?

data Language = Haskell | Bash

parseLanguage :: Text -> Maybe Language
parseLanguage t = case CI.mk t of
    "haskell" -> Just Haskell
    "bash" -> Just Bash
    _ -> Nothing

-- TODO:
--
-- - Skeletons of configuration files would be very useful.  Not only an empty
--   one for the newly created subcommand, but also being able to have various
--   commonly used configuration skeletons.  Or is that something that `config`
--   subcommand should be doing?
