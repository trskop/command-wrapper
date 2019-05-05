-- |
-- Module:      Main
-- Description: CommandWrapper subcommand for generating subcommand skeletons
-- Copyright:   (c) 2018-2019 Peter Trško
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
import Data.Foldable (asum)
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import Data.String (fromString)
import GHC.Generics (Generic)

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

import CommandWrapper.Prelude
    ( HaveCompletionInfo(completionInfoMode)
    , Params(Params, config, name)
    , completionInfoFlag
    , subcommandParams
    , printOptparseCompletionInfoExpression
    , stdout
    )


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

data DefaultModeOptions = DefaultModeOptions
    { subcommandName :: String
    , language :: Maybe Language
    , editAfterwards :: Maybe Bool
    }

data Mode
    = DefaultMode DefaultModeOptions
    | CompletionInfo

instance HaveCompletionInfo Mode where
    completionInfoMode = const CompletionInfo

main :: IO ()
main = do
    params <- subcommandParams
    Turtle.options description parseOptions >>= \case
        DefaultMode opts ->
            mainAction params opts

        CompletionInfo ->
            printOptparseCompletionInfoExpression stdout
  where
    description =
        "Generate subcommand skeleton for specific command-wrapper environment"

mainAction :: Params -> DefaultModeOptions -> IO ()
mainAction Params{name = wrapperName, config = configFile}
  DefaultModeOptions
    { subcommandName
    , language = possiblyLanguage
    , editAfterwards = possiblyEditAfterwards
    }
  = do
        mkConfig <- Dhall.inputFile Dhall.auto configFile

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

generateSkeleton :: Template -> IO FilePath
generateSkeleton Template{executable, targetFile, template} = do
    -- This may be the first command of that specific toolset.
    createDirectoryIfMissing True (takeDirectory targetFile)
    Text.writeFile targetFile template
    when executable $ do
        perms <- getPermissions targetFile
        setPermissions targetFile perms{Directory.executable = True}

    pure targetFile

parseOptions :: Turtle.Parser Mode
parseOptions = asum
    [ completionInfoFlag <*> pure CompletionInfo
    , go
        <$> ( Turtle.arg parseSubcommandName "SUBCOMMAND"
                "Name of the new subcommand"
                <&> \subcommandName -> DefaultModeOptions
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
                <&> \editAfterwards opts@DefaultModeOptions{} ->
                    (opts :: DefaultModeOptions){editAfterwards}
            )

        <*> ( optional
                ( Turtle.switch "no-edit" 'E'
                    "Don't open the created file in an editor afterwards"
                )
                <&> \doNotEditAfterwards opts ->
                    (opts :: DefaultModeOptions)
                        { editAfterwards = not <$> doNotEditAfterwards
                        }
            )
    ]
  where
    go m f g h = DefaultMode $ h (g (f m))  -- TODO: Switch to 'foldEndo'.

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
--
-- - When `TOOLSET skel SUBCOMMAND --language=dhall` is invoked we could try to
--   find a specific skeleton for that subcommand.
--
-- - Maybe switch syntax of config skeletons to:
--
--     ```
--     TOOLSET skel {--config|-c} {--toolset|SUBCOMMAND}
--     ```
