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

import Control.Applicative (many, optional)
import Control.Monad (unless, when)
import Data.Foldable (asum)
import Data.Function (on)
import Data.Functor ((<&>))
import qualified Data.List as List (filter, isPrefixOf)
import Data.Maybe (fromMaybe)
import Data.Monoid (Last(Last, getLast), (<>))
import Data.String (fromString)
import GHC.Generics (Generic)
import System.Environment (getArgs)

import Data.CaseInsensitive as CI (mk)
import Data.Monoid.Endo (Endo(appEndo))
import Data.Monoid.Endo.Fold (foldEndo)
import Data.Text (Text)
import qualified Data.Text.IO as Text (writeFile)
import qualified Dhall (Inject, Interpret, auto, inputFile)
import Data.Text.Prettyprint.Doc ((<+>))
import qualified Data.Text.Prettyprint.Doc as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Pretty (AnsiStyle)
import qualified Data.Text.Prettyprint.Doc.Util as Pretty (reflow)
import qualified Options.Applicative as Options
    ( Parser
    , argument
    , auto
    , defaultPrefs
    , execParserPure
    , flag'
    , handleParseResult
    , help
    , info
    , internal
    , long
    , maybeReader
    , metavar
    , option
    , short
    , strArgument
    , switch
    )
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

import qualified CommandWrapper.Internal.Subcommand.Help as Help
import CommandWrapper.Message (Result, defaultLayoutOptions, message)
import CommandWrapper.Options.Shell (Shell)
import CommandWrapper.Options.Shell as Shell (parse)
import CommandWrapper.Prelude
    ( HaveCompletionInfo(completionInfoMode)
    , Params(Params, colour, config, name, subcommand, verbosity)
    , completionInfoFlag
    , dieWith
    , printCommandWrapperStyleCompletionInfoExpression
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
    , defaultLanguage :: Maybe Language
    , editAfterwards :: Bool
    , editor :: Maybe Editor
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
    | Completion Word Shell [String]
    | Help

instance HaveCompletionInfo Mode where
    completionInfoMode = const CompletionInfo

main :: IO ()
main = do
    params <- subcommandParams
    options <- getArgs

    -- TODO: Switch to custom parser so that errors are printed correctly.
    mode <- Options.handleParseResult
        $ Options.execParserPure Options.defaultPrefs
            (Options.info parseOptions mempty) options
    case mode of
        DefaultMode opts ->
            mainAction params opts

        CompletionInfo ->
            printCommandWrapperStyleCompletionInfoExpression stdout

        Completion index _shell words' ->
            doCompletion params index words'

        Help ->
            let Params{verbosity, colour} = params
             in message defaultLayoutOptions verbosity colour stdout
                    (helpMsg params)

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
            Text.writeFile targetFile template

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

parseOptions :: Options.Parser Mode
parseOptions = asum
    [ completionInfoFlag <*> pure Help
    , Options.flag' Help (Options.short 'h' <> Options.long "help")
    , completionOptions

    , go
        <$> ( Options.argument (Options.maybeReader parseSubcommandName)
                (Options.metavar "SUBCOMMAND")
                <&> \subcommandName -> DefaultModeOptions
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
                <&> \language mode -> mode{language}
            )

        <*> ( optional
                (Options.switch (Options.long "parents" <> Options.short 'p'))
                <&> maybe id \createParents opts ->
                        (opts :: DefaultModeOptions){createParents}
            )

        <*> ( optional
                ( Options.flag' True $ mconcat
                    [ Options.long "edit"
                    , Options.short 'e'
                    , Options.help
                        "Open the created file in an editor afterwards"
                    ]
                )
                <&> \editAfterwards opts ->
                        (opts :: DefaultModeOptions){editAfterwards}
            )

        <*> ( optional
                ( Options.flag' False $ mconcat
                    [ Options.long "no-edit"
                    , Options.short 'E'
                    , Options.help
                        "Don't open the created file in an editor afterwards"
                    ]
                )
                <&> \editAfterwards opts ->
                        (opts :: DefaultModeOptions){editAfterwards}
            )
    ]
  where
    go m f g h i = DefaultMode $ foldEndo f g h i `appEndo` m

completionOptions :: Options.Parser Mode
completionOptions =
    Options.flag' Completion (Options.long "completion" <> Options.internal)
    <*> Options.option Options.auto (Options.long "index" <> Options.internal)
    <*> Options.option (Options.maybeReader $ Shell.parse . CI.mk)
            (Options.long "shell" <> Options.internal)
    <*> many (Options.strArgument (Options.metavar "WORD" <> Options.internal))

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

doCompletion :: Params -> Word -> [String] -> IO ()
doCompletion Params{} index words' =
    mapM_ putStrLn (List.filter (pat `List.isPrefixOf`) allOptions)
  where
    pat = fromMaybe (lastDef "" words') (atMay words' (fromIntegral index))

    allOptions =
        [ "-l", "--language=bash", "--language=dhall", "--language=haskell"
        , "-p", "--parents"
        , "-e", "--edit", "-E", "--no-edit"
        , "-h", "--help"
        ]

helpMsg :: Params -> Pretty.Doc (Result Pretty.AnsiStyle)
helpMsg Params{name, subcommand} = Pretty.vsep
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
        ]

    , Help.section (Help.metavar "SUBCOMMAND")
        [ Pretty.reflow "Name of the new subcommand. Where and how the source\
            \ code or executable file will be named is configurable."
        ]
    , ""
    ]
  where
    subcommand' = fromString subcommand
