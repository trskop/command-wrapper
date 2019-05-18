{-# LANGUAGE TypeFamilies #-}
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
import Control.Monad (unless, when)
import Data.Foldable (asum)
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import Data.String (fromString)
import GHC.Generics (Generic)

import Data.CaseInsensitive as CI (mk)
import Data.Monoid.Endo (Endo(appEndo))
import Data.Monoid.Endo.Fold (foldEndo)
import Data.Text (Text)
import qualified Data.Text.IO as Text (writeFile)
import qualified Dhall (Inject, Interpret, auto, inputFile)
import qualified Data.Text as Text (unpack)
import System.Directory
    ( createDirectoryIfMissing
    , doesDirectoryExist
    , doesPathExist
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
    , dieWith
    , printOptparseCompletionInfoExpression
    , stderr
    , stdout
    , subcommandParams
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
    , createParents :: Bool
    }

data Mode
    = DefaultMode DefaultModeOptions
    | CompletionInfo
    | Completion Word [String]
    | Help

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

        Completion _ _ ->
            notYetImplemented params

        Help ->
            notYetImplemented params
  where
    description =
        "Generate subcommand skeleton for specific command-wrapper environment"

    notYetImplemented params =
        dieWith params stderr 125 "Bug: This is not yet implemented."

mainAction :: Params -> DefaultModeOptions -> IO ()
mainAction
  params@Params
    { name = wrapperName
    , config = configFile
    }
  DefaultModeOptions
    { subcommandName
    , language = possiblyLanguage
    , editAfterwards = possiblyEditAfterwards
    , createParents
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

        createdFile <- generateSkeleton params createParents (template language)

        putStrLn ("Successfully created '" <> createdFile <> "'")

        when (fromMaybe editAfterwards possiblyEditAfterwards)
            $ startEditor createdFile

generateSkeleton :: Params -> Bool -> Template -> IO FilePath
generateSkeleton params createParents Template{..} = do
    let targetDirectory = takeDirectory targetFile
    if createParents
        then createDirectoryIfMissing True targetDirectory
        else do
            targetDirectoryExists <- doesDirectoryExist targetDirectory
            unless targetDirectoryExists $ do
                dieWith params stderr 3
                    $ fromString (show targetDirectory)
                    <> ": Target directory doesn't exist, use '--parents' if\
                        \ you want it to be created for you."

    targetExists <- doesPathExist targetFile
    if targetExists
        then
            dieWith params stderr 3
                $ fromString (show targetFile)
                <> ": Target already exists, refusing to overwrite it."
        else
            Text.writeFile targetFile template

    when executable $ do
        perms <- getPermissions targetFile
        setPermissions targetFile perms{Directory.executable = True}

    pure targetFile

parseOptions :: Turtle.Parser Mode
parseOptions = asum
    [ completionInfoFlag <*> pure Help
    , go
        <$> ( Turtle.arg parseSubcommandName "SUBCOMMAND"
                "Name of the new subcommand"
                <&> \subcommandName -> DefaultModeOptions
                    { subcommandName
                    , language = Nothing
                    , editAfterwards = Nothing
                    , createParents = False
                    }
            )

        <*> ( optional
                ( Turtle.opt parseLanguage "language" 'l'
                    "Choose programming language of subcommand skeleton"
                )
                <&> \language mode -> mode{language}
            )

        <*> ( optional
                ( Turtle.switch "parents" 'p'
                    "Create parent directories if they do not exist"
                )
                <&> maybe id \createParents opts ->
                    (opts :: DefaultModeOptions){createParents}
            )

        <*> ( optional
                ( Turtle.switch "edit" 'e'
                    "Open the created file in an editor afterwards"
                )
                <&> \editAfterwards opts ->
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
    go m f g h i = DefaultMode $ foldEndo f g h i `appEndo` m

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
--
-- - Toolset skeleton, i.e. create:
--
--     - Toolset symbolic link to Command Wrapper.
--     - Lib and config directories.
--     - Initial configuration.
