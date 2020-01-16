{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:      CommandWrapper.Internal.Help
-- Description: Implementation of internal command named help
-- Copyright:   (c) 2018-2020 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions; POSIX.
--
-- Implementation of internal command named @help@.
module CommandWrapper.Internal.Subcommand.Help
    ( help
    , helpSubcommandHelp
    , helpSubcommandCompleter
    )
  where

import Prelude (fromIntegral)

import Control.Applicative ((<*>), (<|>), optional, pure)
import Control.Monad ((>>=))
import Data.Bool (Bool(False, True), otherwise)
import qualified Data.Char as Char (toLower)
import Data.Eq ((==))
import Data.Foldable (any, null, traverse_)
import Data.Function (($), (.), on)
import Data.Functor (Functor, (<$>), (<&>), fmap)
import qualified Data.List as List
    ( deleteBy
    , elem
    , filter
    , intercalate
    , isPrefixOf
    , nub
    , take
    )
import qualified Data.List.NonEmpty as NonEmpty (toList)
import Data.Maybe (Maybe(Just, Nothing), fromMaybe, listToMaybe)
import Data.Monoid (Endo(Endo), mempty)
import Data.Semigroup ((<>))
import Data.String (String, fromString)
import Data.Tuple (fst)
import Data.Word (Word)
import GHC.Generics (Generic)
import System.Environment (getEnvironment)
import System.IO (IO, putStrLn, stderr, stdout)
import Text.Show (Show, show)

import Data.Monoid.Endo.Fold (foldEndo)
import Data.Text.Prettyprint.Doc (Doc, Pretty(pretty), (<+>))
import qualified Data.Text.Prettyprint.Doc as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Pretty
    ( AnsiStyle
    , Color(Magenta, White)
    , color
    , colorDull
    )
import qualified Data.Text.Prettyprint.Doc.Util as Pretty (reflow)
import qualified Mainplate (applySimpleDefaults)
import qualified Options.Applicative as Options
    ( Parser
    , defaultPrefs
    , flag
    , flag'
    , help
    , info
    , long
    , metavar
    , short
    , strArgument
    )
import Safe (atMay, headMay, lastMay)
import System.Directory (findExecutablesInDirectories)
import System.Posix.Process (executeFile)

import CommandWrapper.Config.Global (Config(..), getAliases)
import CommandWrapper.Core.Config.Alias
    ( Alias(Alias, alias, description)
    , applyAlias
    )
import CommandWrapper.Core.Environment (AppNames(AppNames, usedName, names))
import CommandWrapper.Core.Help.Pretty
    ( globalOptionsHelp
    , helpOptions
    , longOption
    , metavar
    , optionDescription
    , section
    , subcommand
    , toolsetCommand
    , usageSection
    )
import CommandWrapper.Core.Message
    ( Result(..)
    , debugMsg
    , defaultLayoutOptions
    , hPutDoc
    , out
    )
import qualified CommandWrapper.External as External
    ( executeCommand
    , findSubcommands
    , getSearchPath
    )
import CommandWrapper.Internal.Utils (runMain)
import qualified CommandWrapper.Options.Optparse as Options
    ( internalSubcommandParse
    )
import qualified CommandWrapper.Options.Shell as Options (Shell)


data HelpMode a
    = MainHelp a
    | SubcommandHelp String a
    | ManPage (Maybe String) a
    | Aliases a
  deriving stock (Functor, Generic, Show)

help
    ::  ( String
        -> Maybe (AppNames -> Config -> Pretty.Doc (Result Pretty.AnsiStyle))
        )
    -- ^ Return help message to print if string argument is an internal
    -- subcommand.
    --
    -- TODO: Return something more structured.
    -> (AppNames -> Config -> Pretty.Doc (Result Pretty.AnsiStyle))
    -- ^ Main\/global help message for the toolset itself.
    -> AppNames
    -> [String]
    -> Config
    -> IO ()
help internalHelp mainHelp appNames@AppNames{usedName} options config =
    runMain (parseOptions appNames config options) defaults $ \case
        MainHelp config' -> do
            out verbosity colourOutput stdout (mainHelp appNames config')
            traverse_ putStrLn extraHelpMessage

        SubcommandHelp cmd config' ->
            case internalHelp cmd of
                Just mkMsg ->
                    out verbosity colourOutput stdout (mkMsg appNames config')

                Nothing ->
                    External.executeCommand appNames config' cmd ["--help"]

        ManPage topic config' -> do
            let internalCommandManPage =
                    ("command-wrapper-" <>) <$> topic

            possiblyManualPageName <- case topic of
                Nothing -> pure (Just usedName)

                Just "command-wrapper" -> pure (Just "command-wrapper")
                Just "completion" -> pure internalCommandManPage
                Just "config" -> pure internalCommandManPage
                Just "help" -> pure internalCommandManPage
                Just "subcommand-protocol" -> pure internalCommandManPage
                Just "version" -> pure internalCommandManPage

                Just subcommandName ->
                    findSubcommandManualPageName appNames config' subcommandName

            case possiblyManualPageName of
                Nothing ->
                    pure () -- TODO: Error message.

                Just manPageName -> do
                    env <- if null manPath
                        then
                            pure Nothing
                        else
                            Just . setManPath <$> getEnvironment

                    executeFile "man" True [manPageName] env

        Aliases config' -> do
            listAliases appNames config'
  where
    defaults = Mainplate.applySimpleDefaults (MainHelp config)

    Config{colourOutput, extraHelpMessage, manPath, verbosity} = config

    setManPath :: [(String, String)] -> [(String, String)]
    setManPath env =
        -- We want to completely override MANPATH environment variable,
        -- otherwise we would have to handle all the interections, and
        -- possiblity of having MANPATH being too long.  At some point we may
        -- want to revisit this.
        ("MANPATH", List.intercalate ":" manPath)
        : List.deleteBy ((==) `on` fst) ("MANPATH", "") env

-- TODO: Consider moving this function or core of its functionality into
-- "CommandWrapper.External" module.
findSubcommandManualPageName
    :: AppNames
    -> Config
    -> String
    -> IO (Maybe String)
findSubcommandManualPageName
  AppNames{usedName, names}
  config@Config{verbosity, colourOutput}
  subcommandName = do
    searchPath <- External.getSearchPath config
    debugMsg (fromString usedName) verbosity colourOutput stderr
        $ "Using following subcommand executable search path: "
        <> fromString (show searchPath)

    loop searchPath (NonEmpty.toList subcommands)
  where
    loop searchPath = \case
        [] -> pure Nothing
        subcmd : untriedSubcmds ->
            findSubcommandExecutable' searchPath subcmd >>= \case
                Just _ -> do
                    debugMsg (fromString usedName) verbosity colourOutput stderr
                        $ fromString (show subcommandName)
                        <> ": Manual page found: "
                        <> fromString (show  subcmd)
                    pure (Just subcmd)

                Nothing -> loop searchPath untriedSubcmds

    findSubcommandExecutable' searchPath =
        fmap listToMaybe . findExecutablesInDirectories searchPath

    subcommands = names <&> \prefix ->
        prefix <> "-" <> subcommandName

parseOptions :: AppNames -> Config -> [String] -> IO (Endo (HelpMode Config))
parseOptions appNames config options =
    execParser $ foldEndo
        <$> optional
                ( subcommandArg
                <|> (manFlag <*> optional subcommandOrTopicArg)
                <|> aliasesFlag
                <|> helpFlag
                )
  where
    switchTo :: (Config -> HelpMode Config) -> Endo (HelpMode Config)
    switchTo f = Endo \case
        MainHelp cfg -> f cfg
        SubcommandHelp _ cfg -> f cfg
        ManPage _ cfg -> f cfg
        Aliases cfg -> f cfg

    manFlag :: Options.Parser (Maybe String -> Endo (HelpMode Config))
    manFlag =
        Options.flag' (\n -> switchTo (ManPage n))
            ( Options.long "man"
            <> Options.help "Show manual page for a SUBCOMMAND or a TOPIC."
            )

    subcommandOrTopicArg :: Options.Parser String
    subcommandOrTopicArg =
        Options.strArgument (Options.metavar "SUBCOMMAND|TOPIC")

    helpFlag :: Options.Parser (Endo (HelpMode Config))
    helpFlag =
        Options.flag mempty (switchTo $ SubcommandHelp "help")
            (Options.long "help" <> Options.short 'h')

    subcommandArg :: Options.Parser (Endo (HelpMode Config))
    subcommandArg =
        Options.strArgument (Options.metavar "SUBCOMMAND") <&> \cmd ->
            let (realCmd, _) = applyAlias (getAliases config) cmd []
            in switchTo (SubcommandHelp realCmd)

    aliasesFlag :: Options.Parser (Endo (HelpMode Config))
    aliasesFlag =
        Options.flag mempty (switchTo Aliases) (Options.long "aliases")

    execParser parser =
        Options.internalSubcommandParse appNames config "help"
            Options.defaultPrefs (Options.info parser mempty) options

helpSubcommandHelp :: AppNames -> Config -> Pretty.Doc (Result Pretty.AnsiStyle)
helpSubcommandHelp AppNames{usedName} _config = Pretty.vsep
    [ Pretty.reflow
        "Display help message for Commnad Wrapper or one of its subcommands."
    , ""

    , usageSection usedName
        [ "help" <+> Pretty.brackets subcommand
        , "help" <+> longOption "man"
            <+> Pretty.brackets
                ( metavar "SUBCOMMAND"
                <> "|"
                <> metavar "TOPIC"
                )
        , "help" <+> longOption "aliases"
        , "help" <+> helpOptions
        , helpOptions
        ]

    , section "Options:"
        [ optionDescription ["SUBCOMMAND"]
            [ Pretty.reflow "Name of an internal or external subcommand for"
            , Pretty.reflow "which to show help message."
            ]

        , optionDescription ["--man [SUBCOMMAND|TOPIC]"]
            [ Pretty.reflow "Show manual page for"
            , metavar "SUBCOMMAND" <> "|" <> metavar "TOPIC"
            , Pretty.reflow "instead of short help message."
            ]

        , optionDescription ["--aliases"]
            [ Pretty.reflow "List and describe available aliases."
            ]

        , optionDescription ["--help", "-h"]
            [ Pretty.reflow "Print this help and exit. Same as"
            , Pretty.squotes (toolsetCommand usedName "help help") <> "."
            ]

        , globalOptionsHelp usedName
        ]

    , ""
    ]

helpSubcommandCompleter
    :: AppNames
    -> Config
    -> Options.Shell
    -> Word
    -> [String]
    -> IO [String]
helpSubcommandCompleter appNames config _shell index words
  | hadAliases  = pure []
  | hadHelp     = pure []
  | hadTopic    = pure []
  | hadDashDash = subcmds
  | null pat    = (helpOptions' <>) <$> subcmds
  | isOption    = pure $ List.filter (pat `List.isPrefixOf`) helpOptions'
  | otherwise   = subcmds
  where
    wordsBeforePattern = List.take (fromIntegral index) words
    hadDashDash = "--" `List.elem` wordsBeforePattern
    hadAliases = "--aliases" `List.elem` wordsBeforePattern
    hadHelp = any (`List.elem` ["--help", "-h"]) wordsBeforePattern
    pat = fromMaybe "" $ atMay words (fromIntegral index)
    isOption = headMay pat == Just '-'
    helpOptions' = ["--aliases", "--help", "-h", "--man", "--"]
    subcmds = findSubcommands appNames config pat

    hadTopic = case lastMay wordsBeforePattern of
        Nothing -> False
        Just ('-' : _) -> False
        _ -> True

-- | Lookup external and internal subcommands matching pattern (prefix).
findSubcommands
    :: AppNames
    -> Config
    -> String
    -- ^ Pattern (prefix) to match subcommand name against.
    -> IO [String]
findSubcommands appNames config pat =
    -- TODO: Function 'findSubcommands' is also defined in
    -- CommandWrapper.Internal.Subcommand.Completion module.
    List.filter (fmap Char.toLower pat `List.isPrefixOf`) <$> getSubcommands
  where
    -- | List all available external and internal subcommands.
    getSubcommands :: IO [String]
    getSubcommands = do
        extCmds <- External.findSubcommands appNames config

        let aliases = alias <$> getAliases config
            -- TODO: Get rid of hardcoded list of internal subcommands.
            internalCommands = ["help", "config", "completion", "version"]

        pure (List.nub $ aliases <> internalCommands <> extCmds)

data AliasAnn
    = AliasName
    | AliasDescription

listAliases :: AppNames -> Config -> IO ()
listAliases AppNames{} config@Config{colourOutput} =
    putDoc $ Pretty.vsep (describe <$> getAliases config) <> Pretty.line
  where
    describe :: Alias -> Doc AliasAnn
    describe Alias{alias, description} =
        Pretty.annotate AliasName (fromString alias)
        <+> Pretty.annotate AliasDescription (pretty description)

    putDoc = hPutDoc defaultLayoutOptions toAnsi colourOutput stdout

    toAnsi = \case
        AliasName -> Pretty.color Pretty.Magenta
        AliasDescription -> Pretty.colorDull Pretty.White
