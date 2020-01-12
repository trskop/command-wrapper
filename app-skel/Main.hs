-- |
-- Module:      Main
-- Description: CommandWrapper subcommand for generating subcommand skeletons
-- Copyright:   (c) 2018-2020 Peter Trško
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

import Prelude hiding (words)

import Control.Applicative (optional)
import Control.Monad (unless, when)
import Data.Function (on)
import Data.Functor ((<&>))
import qualified Data.List as List (filter, isPrefixOf, take)
import Data.Maybe (fromMaybe)
import Data.Monoid (Last(Last, getLast), (<>))
import Data.String (fromString)
import GHC.Generics (Generic)
import System.Environment (getArgs)

import Data.CaseInsensitive as CI (mk)
import Data.Monoid.Endo (Endo)
import Data.Monoid.Endo.Fold (dualFoldEndo)
import Data.Text (Text)
import Dhall (FromDhall, ToDhall)
import qualified Dhall (auto)
import Data.Text.Prettyprint.Doc ((<+>))
import qualified Data.Text.Prettyprint.Doc as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Pretty (AnsiStyle)
import qualified Data.Text.Prettyprint.Doc.Util as Pretty (reflow)
import qualified Options.Applicative as Options
    ( Parser
    , argument
    , flag'
    , help
    , long
    , maybeReader
    , metavar
    , option
    , short
    , switch
    )
import qualified System.AtomicWrite.Writer.Text as Text (atomicWriteFile)
import System.Directory
    ( createDirectoryIfMissing
    , doesDirectoryExist
    , doesPathExist
    , getPermissions
    , setPermissions
    )
import qualified System.Directory as Directory (executable)
import Safe (atMay, lastDef)
import System.Editor
    ( Editor
    , File(..)
    , execEditorCommand
    , getEditorCommand
    , simpleEditorCommand
    , stdEditorLookupStrategy
    )
import qualified System.Editor as Editor (editor)
import System.FilePath (takeDirectory)

import qualified CommandWrapper.Core.Help.Pretty as Help
import qualified CommandWrapper.Subcommand.Prelude (SubcommandProps(..))
import CommandWrapper.Subcommand.Prelude
    ( Params(Params, name, subcommand)
    , Result
    , Shell
    , SubcommandProps(SubcommandProps)
    , dieWith
    , dieMissingConfiguration
    , stderr
    , subcommandParams
    , runSubcommand
    , noPreprocessing
    , inputConfig
    )


data Language
    = Haskell
    | Bash
    | Dhall
  deriving stock (Generic, Show)
  deriving anyclass (Dhall.ToDhall, Dhall.FromDhall)

data Config = Config
    { template :: Language -> Template
    , defaultLanguage :: Maybe Language
    , editAfterwards :: Bool
    , editor :: Maybe Editor
    }
  deriving stock (Generic)
  deriving anyclass (Dhall.FromDhall)

data Template = Template
    { targetFile :: FilePath
    , template :: Text
    , executable :: Bool
    }
  deriving stock (Generic, Show)
  deriving anyclass (Dhall.FromDhall)

data Action = Action
    { subcommandName :: String
    , language :: Maybe Language
    , editAfterwards :: Maybe Bool
    , createParents :: Bool
    }

data SkelParams = SkelParams
    { protocol :: Params
    }
  deriving stock (Generic)

main :: IO ()
main = do
    props <- subcommandProps
    runSubcommand props \SkelParams{protocol} ->
        mainAction protocol
  where
    subcommandProps :: IO (SubcommandProps SkelParams Action)
    subcommandProps = do
        protocol <- subcommandParams
        arguments <- getArgs
        pure SubcommandProps
            { preprocess = noPreprocessing
            , doCompletion
            , helpMsg
            , actionOptions = parseOptions
            , defaultAction = Nothing
            , params = SkelParams{protocol}
            , arguments
            }

mainAction :: Params -> Action -> IO ()
mainAction
  params@Params
    { name = wrapperName
    }
  Action
    { subcommandName
    , language = possiblyLanguage
    , editAfterwards = possiblyEditAfterwards
    , createParents
    }
  = do
        mkConfig <- inputConfig Dhall.auto params >>= \case
            Just config -> pure config
            Nothing     -> dieMissingConfiguration params stderr Nothing

        let subcommand = wrapperName <> "-" <> subcommandName
            Config{template, defaultLanguage, editAfterwards, editor} =
                mkConfig
                    (fromString @Text wrapperName)
                    (fromString @Text subcommandName)
                    (fromString @Text subcommand)

            language = fromMaybe Haskell (defaultLanguage <<>> possiblyLanguage)

        createdFile <- generateSkeleton params createParents (template language)

        putStrLn ("Successfully created '" <> createdFile <> "'")

        when (fromMaybe editAfterwards possiblyEditAfterwards)
            $ startEditor (fromMaybe defaultEditor editor) createdFile
    where
      (<<>>) = (getLast .) . ((<>) `on` Last)

      defaultEditor = Editor.editor (simpleEditorCommand "vi")

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
            Text.atomicWriteFile targetFile template

    when executable $ do
        perms <- getPermissions targetFile
        setPermissions targetFile perms{Directory.executable = True}

    pure targetFile

startEditor :: Editor -> FilePath -> IO ()
startEditor defaultEditor file = do
    editorCommand <- getEditorCommand Nothing (stdEditorLookupStrategy "nvim")
        simpleEditorCommand (pure False)
    execEditorCommand (maybe defaultEditor Editor.editor editorCommand)
        ( Just File{file = fromString file, line = 0}
        )

parseOptions :: Options.Parser (Endo (Maybe Action))
parseOptions = dualFoldEndo
    <$> ( Options.argument (Options.maybeReader parseSubcommandName)
            (Options.metavar "SUBCOMMAND")
            <&> \subcommandName (_ :: Maybe Action) -> Just Action
                    { subcommandName
                    , language = Nothing
                    , editAfterwards = Nothing
                    , createParents = False
                    }
        )

    <*> ( optional
            ( Options.option (Options.maybeReader parseLanguage)
                ( Options.long "language"
                <> Options.short 'l'
                )
            )
            <&> \language -> fmap \mode -> mode{language}
        )

    <*> ( optional
            (Options.switch (Options.long "parents" <> Options.short 'p'))
            <&> maybe id \createParents -> fmap \opts ->
                    (opts :: Action){createParents}
        )

    <*> ( optional
            ( Options.flag' True $ mconcat
                [ Options.long "edit"
                , Options.short 'e'
                , Options.help
                    "Open the created file in an editor afterwards"
                ]
            )
            <&> \editAfterwards -> fmap \opts ->
                    (opts :: Action){editAfterwards}
        )

    <*> ( optional
            ( Options.flag' False $ mconcat
                [ Options.long "no-edit"
                , Options.short 'E'
                , Options.help
                    "Don't open the created file in an editor afterwards"
                ]
            )
            <&> \editAfterwards -> fmap \opts ->
                    (opts :: Action){editAfterwards}
        )

parseSubcommandName :: String -> Maybe String
parseSubcommandName = \case
    "" -> Nothing
    t -> Just t
        -- TODO: Check that subcommand name consists of only reasonable ASCII
        --       characters. Any other reasonable restrictions on subcommand
        --       name?

parseLanguage :: String -> Maybe Language
parseLanguage t = case CI.mk t of
    "haskell" -> Just Haskell
    "bash" -> Just Bash
    "dhall" -> Just Dhall
    _ -> Nothing

doCompletion :: SkelParams -> Word -> Shell -> [String] -> IO ()
doCompletion SkelParams{} index _shell words =
    mapM_ putStrLn $ List.filter (pat `List.isPrefixOf`) if
      | null wordsBeforePattern ->
            mconcat [helpOptions, parentsOptions, languageOptions, editOptions]

      | Just "-l" <- atMay words (fromIntegral index - 1) ->
            languages

      | hadHelpOption ->
            []

      | otherwise ->
            mconcat
                [ if hadParentsOption then [] else parentsOptions
                , if hadLanguageOption then [] else languageOptions
                , editOptions
                ]
  where
    pat = fromMaybe (lastDef "" words) (atMay words (fromIntegral index))

    wordsBeforePattern = List.take (fromIntegral index) words

    editOptions =  ["-e", "--edit", "-E", "--no-edit"]

    hadHelpOption = any (`elem` helpOptions) wordsBeforePattern

    helpOptions = ["-h", "--help"]

    hadParentsOption = any (`elem` parentsOptions) wordsBeforePattern

    parentsOptions = ["-p", "--parents" ]

    hadLanguageOption = any (`elem` languageOptions) wordsBeforePattern

    languageOptions = ("-l" : ["--language=" <> lang | lang <- languages])

    languages = ["bash", "dhall", "haskell"]

helpMsg :: SkelParams -> Pretty.Doc (Result Pretty.AnsiStyle)
helpMsg SkelParams{protocol = Params{name, subcommand}} = Pretty.vsep
    [ Pretty.reflow "Generate subcommand skeleton for specific command-wrapper\
        \ environment."
    , ""

    , Help.usageSection name
        [ subcommand'
            <+> Pretty.brackets
                    (Help.longOptionWithArgument "language" "LANGUAGE")
            <+> Pretty.brackets
                    ( Help.longOption "parents"
                    <> "|"
                    <> Help.longOption "[no-]edit"
                    )
            <+> Help.metavar "SUBCOMMAND"

        , subcommand' <+> Help.helpOptions

        , "help" <+> Pretty.brackets (Help.longOption "man") <+> subcommand'
        ]

    , Help.section ("Options" <> ":")
        [ Help.optionDescription ["--language=LANGUAGE", "-l LANGUAGE"]
            [ Pretty.reflow "Choose the programming"
            , Help.metavar "LANGUAGE", "of", Help.metavar "SUBCOMMAND"
            , "skeleton."
            ]

        , Help.optionDescription ["--parents", "-p"]
            [ Pretty.reflow "Create parent directories if they do not exist."
            ]

        , Help.optionDescription ["--[no-]edit", "-e", "-E"]
            [ Pretty.reflow
                "Open, or not, the created file in an editor afterwards."
            , "Options", Help.shortOption 'e', "and", Help.shortOption 'E'
            , Pretty.reflow "are equivalent to", Help.longOption "edit", "and"
            , Help.longOption "no-edit" <> ",", "respectively."
            ]

        , Help.optionDescription ["--help", "-h"]
            [ Pretty.reflow "Print this help and exit. Same as"
            , Pretty.squotes
                (Help.toolsetCommand name ("help" <+> subcommand')) <> "."
            ]

        , Help.optionDescription ["SUBCOMMAND"]
            [ Pretty.reflow "Name of the new subcommand. Where and how the\
                \ source code or executable file will be named is\
                \ configurable."
            ]

        , Help.globalOptionsHelp name
        ]
    ]
  where
    subcommand' = fromString subcommand
