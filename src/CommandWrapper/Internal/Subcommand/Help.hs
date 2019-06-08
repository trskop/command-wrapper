{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:      CommandWrapper.Internal.Help
-- Description: Implementation of internal command named help
-- Copyright:   (c) 2018-2019 Peter Trško
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

    -- * Utilities
    , (<++>)
    , command
    , dullGreen
    , globalOptions
    , globalOptionsHelp
    , helpOptions
    , longOption
    , longOptionWithArgument
    , metavar
    , optionDescription
    , optionalMetavar
    , optionalSubcommand
    , section
    , shortOption
    , subcommand
    , toolsetCommand
    , usageSection
    , value
    )
  where

import Prelude (fromIntegral)

import Control.Applicative ((<*>), (<|>), optional, pure)
import Control.Monad ((>>=))
import Data.Bool (Bool(False, True), otherwise)
import Data.Char (Char)
import qualified Data.Char as Char (toLower)
import Data.Eq ((==))
import Data.Foldable (null, traverse_)
import Data.Function (($), (.))
import Data.Functor (Functor, (<$>), (<&>), fmap)
import qualified Data.List as List (elem, filter, isPrefixOf, nub, take)
import qualified Data.List.NonEmpty as NonEmpty (toList)
import Data.Maybe (Maybe(Just, Nothing), fromMaybe, listToMaybe)
import Data.Monoid (Endo(Endo), mempty)
import Data.Semigroup ((<>))
import Data.String (String, fromString)
import Data.Word (Word)
import GHC.Generics (Generic)
import System.IO (IO, putStrLn, stderr, stdout)
import Text.Show (Show, show)

import Data.Monoid.Endo.Fold (foldEndo)
import Data.Text (Text)
import Data.Text.Prettyprint.Doc (Pretty(pretty), (<+>))
import qualified Data.Text.Prettyprint.Doc as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Pretty
    ( AnsiStyle
    , Color(Green, Magenta)
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
import CommandWrapper.Environment (AppNames(AppNames, usedName, names))
import qualified CommandWrapper.External as External
    ( executeCommand
    , findSubcommands
    , getSearchPath
    )
import CommandWrapper.Internal.Utils (runMain)
import CommandWrapper.Message
    ( Result(..)
    , debugMsg
    , defaultLayoutOptions
    , message
    )
import CommandWrapper.Options.Alias (Alias(alias), applyAlias)
import qualified CommandWrapper.Options.Optparse as Options
    ( internalSubcommandParse
    )
import qualified CommandWrapper.Options.Shell as Options (Shell)


data HelpMode a
    = MainHelp a
    | SubcommandHelp String a
    | ManPage (Maybe String) a
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
            message defaultLayoutOptions verbosity colourOutput stdout
                (mainHelp appNames config')
            traverse_ putStrLn extraHelpMessage

        SubcommandHelp cmd config' ->
            case internalHelp cmd of
                Just mkMsg ->
                    message defaultLayoutOptions verbosity colourOutput stdout
                        (mkMsg appNames config')

                Nothing ->
                    External.executeCommand appNames config' cmd ["--help"]

        -- TODO:
        -- - We need to take aliases into account.
        -- - We need to extend completion to list "subcommand-protocol",
        --   "command-wrapper", etc.
        -- - We need to generalise our approach so that any other
        --   "command-wrapper-${TOPIC}" or "${TOOLSET}-${TOPIC}" manual
        --   pages are also accessible.
        -- - When no subcommand/topic name is give we should be able to default
        --   to "command-wrapper" if a more concrete manual page desn't exist.
        --   At the moment we assume that manual page for toolset is always
        --   present.
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
                Nothing -> pure () -- TODO: Error message.
                Just manPageName ->
                    executeFile "man" True [manPageName] Nothing
  where
    defaults = Mainplate.applySimpleDefaults (MainHelp config)

    Config{colourOutput, extraHelpMessage, verbosity} = config

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

-- TODO:
--
-- > TOOLSET [GLOBAL_OPTIONS] help [SUBCOMMAND]
-- > TOOLSET [GLOBAL_OPTIONS] help --man [SUBCOMMAND|TOPIC]
parseOptions :: AppNames -> Config -> [String] -> IO (Endo (HelpMode Config))
parseOptions appNames config options =
    execParser $ foldEndo
        <$> optional
                ( subcommandArg
                <|> (manFlag <*> optional subcommandOrTopicArg)
                <|> helpFlag
                )
  where
    switchTo :: (Config -> HelpMode Config) -> Endo (HelpMode Config)
    switchTo f = Endo \case
        MainHelp cfg -> f cfg
        SubcommandHelp _ cfg -> f cfg
        ManPage _ cfg -> f cfg

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

        , optionDescription ["--help", "-h"]
            [ Pretty.reflow "Print this help and exit. Same as"
            , Pretty.squotes (toolsetCommand usedName "help help") <> "."
            ]

        , globalOptionsHelp usedName
        ]

    , ""
    ]

helpOptions :: Pretty.Doc (Result Pretty.AnsiStyle)
helpOptions = Pretty.braces (longOption "help" <> "|" <> shortOption 'h')

subcommand :: Pretty.Doc (Result Pretty.AnsiStyle)
subcommand = metavar "SUBCOMMAND"

optionalSubcommand :: Pretty.Doc (Result Pretty.AnsiStyle)
optionalSubcommand = Pretty.brackets subcommand

globalOptions :: Pretty.Doc (Result Pretty.AnsiStyle)
globalOptions = Pretty.brackets (Pretty.annotate dullGreen "GLOBAL_OPTIONS")

dullGreen, magenta :: Result Pretty.AnsiStyle
dullGreen = Result (Pretty.colorDull Pretty.Green)
magenta = Result (Pretty.color Pretty.Magenta)

usageSection
    :: Pretty command
    => command
    -> [Pretty.Doc (Result Pretty.AnsiStyle)]
    -> Pretty.Doc (Result Pretty.AnsiStyle)
usageSection commandName ds =
    section "Usage:"
        $   ( ds <&> \rest ->
                pretty commandName <+> globalOptions <+> rest
            )
        <>  [""]

globalOptionsHelp
    :: Pretty toolset
    => toolset
    -> Pretty.Doc (Result Pretty.AnsiStyle)
globalOptionsHelp toolset =
    optionDescription ["GLOBAL_OPTIONS"]
        [ Pretty.reflow "See output of" <++> Pretty.squotes callHelp <> "."
        ]
  where
    callHelp = toolsetCommand toolset "help"

section :: Pretty.Doc ann -> [Pretty.Doc ann] -> Pretty.Doc ann
section d ds = Pretty.nest 2 $ Pretty.vsep (d : "" : ds)

optionDescription
    :: [Text]
    -> [Pretty.Doc (Result Pretty.AnsiStyle)]
    -> Pretty.Doc (Result Pretty.AnsiStyle)
optionDescription opts ds = Pretty.vsep
    [ prettyOpts opts
    , Pretty.indent 4 $ Pretty.fillSep ds
    , ""
    ]
  where
    prettyOpts =
        Pretty.hsep
        . Pretty.punctuate Pretty.comma
        . fmap (Pretty.annotate dullGreen . pretty)

shortOption :: Char -> Pretty.Doc (Result Pretty.AnsiStyle)
shortOption o = Pretty.annotate dullGreen ("-" <> pretty o)

longOption :: Text -> Pretty.Doc (Result Pretty.AnsiStyle)
longOption o = Pretty.annotate dullGreen ("--" <> pretty o)

longOptionWithArgument :: Text -> Text -> Pretty.Doc (Result Pretty.AnsiStyle)
longOptionWithArgument o a = longOption o <> "=" <> metavar a

metavar :: Text -> Pretty.Doc (Result Pretty.AnsiStyle)
metavar = Pretty.annotate dullGreen . pretty

value :: Text -> Pretty.Doc (Result Pretty.AnsiStyle)
value = Pretty.annotate dullGreen . pretty

optionalMetavar :: Text -> Pretty.Doc (Result Pretty.AnsiStyle)
optionalMetavar = Pretty.brackets . metavar

command
    :: Pretty.Doc (Result Pretty.AnsiStyle)
    -> Pretty.Doc (Result Pretty.AnsiStyle)
command = Pretty.annotate magenta

toolsetCommand
    :: Pretty toolset
    => toolset
    -> Pretty.Doc (Result Pretty.AnsiStyle)
    -> Pretty.Doc (Result Pretty.AnsiStyle)
toolsetCommand toolset doc = Pretty.annotate magenta (pretty toolset <+> doc)

(<++>) :: Pretty.Doc ann -> Pretty.Doc ann -> Pretty.Doc ann
x <++> y = x <> Pretty.softline <> y

helpSubcommandCompleter
    :: AppNames
    -> Config
    -> Options.Shell
    -> Word
    -> [String]
    -> IO [String]
helpSubcommandCompleter appNames config _shell index words
  | hadTopic    = pure []
  | hadDashDash = subcmds
  | null pat    = (helpOptions' <>) <$> subcmds
  | isOption    = pure $ List.filter (pat `List.isPrefixOf`) helpOptions'
  | otherwise   = subcmds
  where
    wordsBeforePattern = List.take (fromIntegral index) words
    hadDashDash = "--" `List.elem` wordsBeforePattern
    pat = fromMaybe "" $ atMay words (fromIntegral index)
    isOption = headMay pat == Just '-'
    helpOptions' = ["--help", "-h", "--"]
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
