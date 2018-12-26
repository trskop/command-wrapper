{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
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
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import Data.String (fromString)
import GHC.Generics (Generic)
import System.Exit (die)

import Data.CaseInsensitive as CI (mk)
import Data.Text (Text)
import qualified Data.Text.IO as Text (writeFile)
import qualified Dhall (Inject, Interpret, auto, inputFile)
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


data Language
    = Haskell
    | Bash
    | Dhall
  deriving stock (Generic, Show)
  deriving anyclass (Dhall.Inject, Dhall.Interpret)

data Config = Config
    { template :: Language -> Template
    , editAfterwards :: Bool
    -- TODO: Editor settings to override environment.
    }
  deriving stock (Generic)
  deriving anyclass (Dhall.Interpret)

data Template = Template
    { targetFile :: FilePath
    , template :: Text
    , executable :: Bool
    }
  deriving stock (Generic, Show)
  deriving anyclass (Dhall.Interpret)

main :: IO ()
main = do
    Environment.Params{name = wrapperName, config = configFile} <- parseEnv
    mkConfig <- Dhall.inputFile Dhall.auto configFile
    Mode
        { subcommandName
        , language = possiblyLanguage
        , editAfterwards = possiblyEditAfterwards
        } <- Turtle.options description parseOptions

    let subcommand = wrapperName <> "-" <> subcommandName
        Config{template, editAfterwards} =
            mkConfig
                (fromString @Text wrapperName)
                (fromString @Text subcommandName)
                (fromString @Text subcommand)

        language = fromMaybe Haskell possiblyLanguage

    createdFile <- generateSkeleton (template language)

    putStrLn ("Successfully created '" <> createdFile <> "'")

    when (fromMaybe editAfterwards possiblyEditAfterwards)
        $ startEditor createdFile
  where
    description =
        "Generate subcommand skeleton for specific command-wrapper environment"

generateSkeleton :: Template -> IO FilePath
generateSkeleton Template{executable, targetFile, template} = do
    -- This may be the first command of that specific toolset.
    createDirectoryIfMissing True (takeDirectory targetFile)
    Text.writeFile targetFile template
    when executable $ do
        perms <- getPermissions targetFile
        setPermissions targetFile perms{Directory.executable = True}

    pure targetFile

parseEnv :: IO Environment.Params
parseEnv = Environment.parseEnvIO (die . show) Environment.askParams

data Mode = Mode
    { subcommandName :: String
    , language :: Maybe Language
    , editAfterwards :: Maybe Bool
    }

parseOptions :: Turtle.Parser Mode
parseOptions =
    go  <$> ( Turtle.arg parseSubcommandName "SUBCOMMAND"
                "Name of the new subcommand"
                <&> \subcommandName -> Mode
                    { subcommandName
                    , language = Nothing
                    , editAfterwards = Nothing
                    }
            )

        <*> ( optional
                ( Turtle.opt parseLanguage "language" 'l'
                    "Choose programming language of subcommand skeleton"
                )
                <&> \language mode -> mode{language}
            )

        <*> ( optional
                ( Turtle.switch "edit" 'e'
                    "Open the created file in an editor afterwards"
                )
                <&> \editAfterwards Mode{language, subcommandName} ->
                    Mode{editAfterwards, language, subcommandName}
            )

        <*> ( optional
                ( Turtle.switch "no-edit" 'E'
                    "Don't open the created file in an editor afterwards"
                )
                <&> \doNotEditAfterwards Mode{language, subcommandName} ->
                    Mode
                        { subcommandName
                        , language
                        , editAfterwards = not <$> doNotEditAfterwards
                        }
            )

  where
    go m f g h = h (g (f m))    -- TODO: Switch to 'foldEndo'.

parseSubcommandName :: Text -> Maybe String
parseSubcommandName = \case
    "" -> Nothing
    t -> Just (Text.unpack t)
        -- TODO: Check that subcommand name consists of only reasonable ASCII
        --       characters. Any other reasonable restrictions on subcommand
        --       name?

parseLanguage :: Text -> Maybe Language
parseLanguage t = case CI.mk t of
    "haskell" -> Just Haskell
    "bash" -> Just Bash
    "dhall" -> Just Dhall
    _ -> Nothing

startEditor :: FilePath -> IO ()
startEditor _ = pure () -- TODO: Implement

-- TODO:
--
-- - Skeletons of configuration files would be very useful.  Not only an empty
--   one for the newly created subcommand, but also being able to have various
--   commonly used configuration skeletons.  Or is that something that `config`
--   subcommand should be doing?
