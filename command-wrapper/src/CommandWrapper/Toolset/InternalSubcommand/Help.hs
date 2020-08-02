-- |
-- Module:      $Header$
-- Description: Implementation of internal command named help
-- Copyright:   (c) 2018-2020 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions; POSIX.
--
-- Implementation of internal command named @help@.
module CommandWrapper.Toolset.InternalSubcommand.Help
    ( HelpOptions(..)
    , SubcommandDescription(..)
    , help
    , helpSubcommandDescription
    , helpSubcommandHelp
    , helpSubcommandCompleter

    , TreeOptions(..)
    , TreeAnn(..)
    , treeAnnToAnsi
    , commandTree
    )
  where

import Prelude (fromIntegral)

import Control.Applicative ((*>), (<*>), (<|>), optional, pure, some)
import Control.Monad ((>>=), guard)
import Data.Bifunctor (Bifunctor(bimap), first)
import Data.Bool (Bool(False, True), not, otherwise)
import Data.Char (Char)
import qualified Data.Char as Char (toLower)
import Data.Eq ((/=), (==))
import Data.Foldable (any, null, traverse_)
import Data.Function (($), (.), on)
import Data.Functor (Functor, (<$), (<$>), (<&>), fmap)
import qualified Data.List as List
    ( deleteBy
    , drop
    , elem
    , filter
    , groupBy
    , intercalate
    , isPrefixOf
    , nub
    , nubBy
    , null
    , repeat
    , sortBy
    , take
    , takeWhile
    , zipWith
    )
import qualified Data.List.NonEmpty as NonEmpty (toList)
import Data.Maybe (Maybe(Just, Nothing), fromMaybe, listToMaybe)
import Data.Monoid (Endo(Endo), mempty, mconcat)
import Data.Ord (compare)
import Data.Semigroup ((<>))
import Data.String (String, fromString, lines, unwords)
import Data.Traversable (for)
import Data.Tuple (fst)
import Data.Word (Word)
import GHC.Generics (Generic)
import System.Environment (getEnvironment)
import System.IO (Handle, IO, putStrLn, stderr, stdout)
import Text.Show (Show, show)

import Data.Monoid.Endo.Fold (foldEndo)
import Data.Text (Text)
import qualified Data.Text as Text (split)
import Data.Text.Prettyprint.Doc (Doc, Pretty(pretty), (<+>))
import qualified Data.Text.Prettyprint.Doc as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Pretty
    ( AnsiStyle
    , Color(Blue, Magenta, White)
    , color
    , colorDull
    )
import qualified Data.Text.Prettyprint.Doc.Util as Pretty (reflow)
import Data.Tree (Forest, Tree(Node), unfoldForest)
import qualified Mainplate (applySimpleDefaults)
import qualified Options.Applicative as Options
    ( Parser
    , defaultPrefs
    , flag
    , flag'
    , info
    , long
    , metavar
    , strArgument
    )
import Safe (atMay, headMay, lastMay)
import System.Directory (findExecutablesInDirectories)
import System.Posix.Process (executeFile)
import System.Process (CreateProcess(env), readCreateProcess)
import qualified System.Process as Process (proc)

import CommandWrapper.Core.Config.Alias
    ( Alias(Alias, alias, command, description)
    , applyAlias
    )
import CommandWrapper.Core.Config.Shell (Shell)
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
    , value
    )
import CommandWrapper.Core.Message
    ( Result(..)
    , debugMsg
    , defaultLayoutOptions
    , hPutDoc
    , out
    )
import CommandWrapper.Toolset.Config.Global
    ( Config
        ( Config
        , colourOutput
        , extraHelpMessage
        , ignoreAliases
        , manPath
        , verbosity
        )
    , getAliases
    , getSearchPath
    )
import qualified CommandWrapper.Toolset.ExternalSubcommand as External
    ( executeCommand
    , executeCommandWith
    , findSubcommands
    )
import CommandWrapper.Toolset.InternalSubcommand.Utils (runMain)
import qualified CommandWrapper.Toolset.InternalSubcommand.Utils as Options
    ( helpFlag
    )
import qualified CommandWrapper.Toolset.Options.Optparse as Options
    ( internalSubcommandParse
    )


data HelpMode a
    = MainHelp a
    | SubcommandHelp String a
    | ManPage (Maybe String) a
    | Search [String] a
    | List ListOptions a
  deriving stock (Functor, Generic, Show)

data ListView
    = PrettyList
    | PrettyTree
  deriving stock (Generic, Show)

data ListOptions = ListOptions
    { view :: ListView
    , includeAliases :: Bool
    , includeInternalSubcommands :: Bool
    , includeExternalSubcommands :: Bool
    , treeOptions :: TreeOptions
    , output :: Handle
    }
  deriving stock (Generic, Show)

defListOptions :: ListView -> ListOptions
defListOptions view = ListOptions
    { view
    , includeAliases = True
    , includeInternalSubcommands = True
    , includeExternalSubcommands = True
    , treeOptions = TreeOptions
        { delimiter = '.'
        , showDescription = True
        }
    , output = stdout
    }

data SubcommandDescription name description = SubcommandDescription
    { name :: name
    , description :: description
    }
  deriving stock (Functor, Generic, Show)

instance Bifunctor SubcommandDescription where
    bimap
        :: (a -> b)
        -> (c -> d)
        -> SubcommandDescription a c
        -> SubcommandDescription b d
    bimap f g SubcommandDescription{name, description} = SubcommandDescription
        { name = f name
        , description = g description
        }

data HelpOptions = HelpOptions
    { internalHelp
        :: String
        -> Maybe (AppNames -> Config -> Pretty.Doc (Result Pretty.AnsiStyle))
    -- ^ Return help message to print if string argument is an internal
    -- subcommand.
    --
    -- TODO: Return something more structured.

    , mainHelp :: AppNames -> Config -> Pretty.Doc (Result Pretty.AnsiStyle)
    -- ^ Main\/global help message for the toolset itself.

    , internalSubcommands :: [SubcommandDescription String String]
    -- ^ List of internal subcommands.  Help subcommand itself has to be on the
    -- list so that its name is not hardcoded.

    , topics :: [String]
    -- ^ Additional help topics that are not toolsets, internal subcommands, or
    -- external subcommands.

    , toManualPage :: String -> Maybe String
    -- ^ Convert values of 'helpTopics' and 'internalSubcommands' into a manual
    -- page names.
    --
    -- Invariant:
    --
    -- > forall s.
    -- >   (s `elem` helpTopics || s `elem` internalSubcommands s)
    -- >      === isJust (toManualPage s)
    }

help
    :: HelpOptions
    -> AppNames
    -> [String]
    -> Config
    -> IO ()
help HelpOptions{..} appNames@AppNames{usedName} options globalConfig =
    runMain (parseOptions appNames globalConfig options) defaults \case
        MainHelp config@Config{colourOutput, extraHelpMessage, verbosity} -> do
            out verbosity colourOutput stdout (mainHelp appNames config)
            traverse_ putStrLn extraHelpMessage

        SubcommandHelp cmd config@Config{colourOutput, verbosity} ->
            case internalHelp cmd of
                Just mkMsg ->
                    out verbosity colourOutput stdout (mkMsg appNames config)

                Nothing ->
                    External.executeCommand appNames config cmd ["--help"]

        ManPage topic config -> do
            possiblyManualPageName <- case topic of
                -- Manual page for the toolset itself.
                Nothing ->
                    pure (toManualPage usedName)

                Just s | pageName@(Just _) <- toManualPage s ->
                    pure pageName

                Just subcommandName ->
                    findSubcommandManualPageName appNames config subcommandName

            case possiblyManualPageName of
                Nothing ->
                    pure () -- TODO: Error message.

                Just manPageName ->
                    manEnvironment config
                        >>= executeFile "man" True [manPageName]

        Search expressions config -> do
            manEnvironment config >>= executeFile "apropos" True expressions

        List opts@ListOptions{view} config -> case view of
            PrettyList ->
                describeSubcommands internalSubcommands opts appNames config

            PrettyTree ->
                subcommandTree internalSubcommands opts appNames config
  where
    defaults = Mainplate.applySimpleDefaults (MainHelp globalConfig)

    manEnvironment :: Config -> IO (Maybe [(String, String)])
    manEnvironment config@Config{manPath}
      | null manPath =
            pure Nothing
      | otherwise = do
            debugPrintManPath config
            Just . setManPath config <$> getEnvironment

    debugPrintManPath :: Config -> IO ()
    debugPrintManPath Config{colourOutput, manPath, verbosity} =
        debugMsg (fromString usedName) verbosity colourOutput stderr
        $ "Using following man path: "
        <> fromString (show (List.intercalate ":" manPath))

    setManPath :: Config -> [(String, String)] -> [(String, String)]
    setManPath Config{manPath} env =
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
    searchPath <- getSearchPath config
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
                <|> (searchFlag <*> some keywordOrRegexpArg)
                <|> (listFlag <*> pure (mempty @(Endo ListOptions)))
                <|> (treeFlag <*> pure (mempty @(Endo ListOptions)))
                <|> aliasesFlag
                <|> Options.helpFlag (switchTo $ SubcommandHelp "help")
                )
  where
    switchTo :: (Config -> HelpMode Config) -> Endo (HelpMode Config)
    switchTo f = Endo \case
        MainHelp cfg -> f cfg
        SubcommandHelp _ cfg -> f cfg
        ManPage _ cfg -> f cfg
        Search _ cfg -> f cfg
        List _ cfg -> f cfg

    manFlag :: Options.Parser (Maybe String -> Endo (HelpMode Config))
    manFlag = Options.flag' (\n -> switchTo (ManPage n)) (Options.long "man")

    searchFlag :: Options.Parser ([String] -> Endo (HelpMode Config))
    searchFlag = Options.flag' (switchTo . Search) (Options.long "search")

    subcommandOrTopicArg :: Options.Parser String
    subcommandOrTopicArg =
        Options.strArgument (Options.metavar "SUBCOMMAND|TOPIC")

    keywordOrRegexpArg :: Options.Parser String
    keywordOrRegexpArg =
        Options.strArgument (Options.metavar "{KEYWORD|REGEXP}")

    subcommandArg :: Options.Parser (Endo (HelpMode Config))
    subcommandArg =
        Options.strArgument (Options.metavar "SUBCOMMAND") <&> \cmd ->
            let (realCmd, _) = applyAlias (getAliases config) cmd []
            in switchTo (SubcommandHelp realCmd)

    aliasesFlag :: Options.Parser (Endo (HelpMode Config))
    aliasesFlag =
        Options.flag mempty (switchTo aliasesMode) (Options.long "aliases")
      where
        aliasesMode = listMode PrettyList $ Endo \opts -> opts
            { includeAliases = True
            , includeExternalSubcommands = False
            , includeInternalSubcommands = False
            }

    listFlag :: Options.Parser (Endo ListOptions -> Endo (HelpMode Config))
    listFlag =
        Options.flag' (switchTo . listMode PrettyList) (Options.long "list")

    treeFlag :: Options.Parser (Endo ListOptions -> Endo (HelpMode Config))
    treeFlag =
        Options.flag' (switchTo . listMode PrettyTree) (Options.long "tree")

    listMode :: ListView -> Endo ListOptions -> config -> HelpMode config
    listMode view (Endo f) = List (f (defListOptions view))

    execParser parser =
        Options.internalSubcommandParse appNames config "help"
            Options.defaultPrefs (Options.info parser mempty) options

helpSubcommandDescription :: String
helpSubcommandDescription =
    "Display help message for Commnad Wrapper or one of its subcommands."

helpSubcommandHelp :: AppNames -> Config -> Pretty.Doc (Result Pretty.AnsiStyle)
helpSubcommandHelp AppNames{usedName} _config = Pretty.vsep
    [ Pretty.reflow (fromString helpSubcommandDescription)
    , ""

    , usageSection usedName
        [ "help" <+> Pretty.brackets subcommand
        , "help" <+> longOption "man"
            <+> Pretty.brackets
                ( metavar "SUBCOMMAND"
                <> "|"
                <> metavar "TOPIC"
                )
        , "help" <+> longOption "search"
            <+> Pretty.braces
                ( metavar "KEYWORD"
                <> "|"
                <> metavar "REGEXP"
                )
            <+> Pretty.brackets "..."
        , "help"
            <+> Pretty.braces
                ( longOption "list"
                <> "|"
                <> longOption "tree"
                <> "|"
                <> longOption "aliases"
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

        , optionDescription ["--search {TOPIC|REGEXP} [...]"]
            [ Pretty.reflow "Search documentation for specified"
            , metavar "TOPIC" <> "|" <> metavar "REGEX" <> "."
            ]

        , optionDescription ["--list"]
            [ Pretty.reflow
                "List and describe all available subcommands including aliases."
            ]

        , optionDescription ["--tree"]
            [ Pretty.reflow
                "List and describe all available subcommands including\
                \ aliases in tree representation using"
            , "'" <> value "." <> "'"
            , Pretty.reflow "character as a separator."
            ]

        , optionDescription ["--aliases"]
            [ Pretty.reflow "List and describe available aliases, otherwise\
                \ it's the same as"
            , longOption "list" <> "."
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
    :: HelpOptions
    -> AppNames
    -> Config
    -> Shell
    -> Word
    -> [String]
    -> IO [String]
helpSubcommandCompleter HelpOptions{internalSubcommands, topics} appNames
  config _shell index words
  | hadMan        = (specialManPages <>) <$> subcmds True
  | hadSearch     = (specialManPages <>) <$> subcmds False
  | hadListOrTree = pure []
  | hadHelp       = pure []
  | hadTopic      = pure []
  | hadDashDash   = subcmds False
  | null pat      = (helpOptions' <>) <$> subcmds False
  | isOption      = pure $ List.filter (pat `List.isPrefixOf`) helpOptions'
  | otherwise     = subcmds False
  where
    wordsBeforePattern = List.take (fromIntegral index) words
    hadDashDash = "--" `List.elem` wordsBeforePattern
    hadHelp = any (`List.elem` ["--help", "-h"]) wordsBeforePattern
    hadMan = any (== "--man") wordsBeforePattern
    pat = fromMaybe "" $ atMay words (fromIntegral index)
    isOption = headMay pat == Just '-'
    hadSearch = "--search" `List.elem` wordsBeforePattern

    hadListOrTree =
        any (`List.elem` ["--aliases", "--list", "--tree"]) wordsBeforePattern

    helpOptions' =
        [ "--aliases"
        , "--help", "-h"
        , "--list"
        , "--man"
        , "--search"
        , "--tree"
        , "--"
        ]

    subcmds ignoreAliases =
        findSubcommands internalSubcommands appNames config{ignoreAliases} pat

    specialManPages =
        List.filter (fmap Char.toLower pat `List.isPrefixOf`) topics

    hadTopic = case lastMay wordsBeforePattern of
        Nothing -> False
        Just ('-' : _) -> False
        _ -> True

-- | Lookup external and internal subcommands and aliases matching pattern
-- (prefix).
findSubcommands
    :: [SubcommandDescription String String]
    -- ^ Internal subcommand names.
    -> AppNames
    -> Config
    -> String
    -- ^ Pattern (prefix) to match subcommand name against.
    -> IO [String]
findSubcommands internalSubcommands appNames config pat =
    -- TODO: Function 'findSubcommands' is also defined in
    -- CommandWrapper.Internal.Subcommand.Completion module.
    if null pat
        then
            getSubcommands
        else
            List.filter (fmap Char.toLower pat `List.isPrefixOf`)
            <$> getSubcommands
  where
    -- | List all available external and internal subcommands.
    getSubcommands :: IO [String]
    getSubcommands = do
        extCmds <- External.findSubcommands appNames config

        let aliases = alias <$> getAliases config

        pure (List.nub $ aliases <> internalSubcommandNames <> extCmds)

    internalSubcommandNames =
        internalSubcommands <&> \SubcommandDescription{name} -> name

getSubcommandDescriptions
    :: [SubcommandDescription String String]
    -> (SubcommandDescription String String -> a)
    -> (Alias -> a)
    -> ListOptions
    -> AppNames
    -> Config
    -> IO [a]
getSubcommandDescriptions
  internalSubcommands
  convertSubcommand
  convertAlias
  ListOptions{..}
  appNames
  config = do
    externalSubcommands <- if includeExternalSubcommands
        then describeExternalSubcommands appNames config
        else pure []
    pure $ mconcat
        [ do
            guard includeAliases
            convertAlias <$> getAliases config

        , do
            guard includeInternalSubcommands
            convertSubcommand <$> internalSubcommands

        , do
            guard includeExternalSubcommands
            convertSubcommand <$> externalSubcommands
        ]

describeExternalSubcommands
    :: AppNames
    -> Config
    -> IO [SubcommandDescription String String]
describeExternalSubcommands appNames config = do
    names <- External.findSubcommands appNames config
    for names \name -> do
        helpMessage <- External.executeCommandWith readProcess' appNames
            config name ["--help"]

        pure SubcommandDescription
            { name
            , description = unwords
                $ List.takeWhile (not . List.null) (lines helpMessage)
            }
  where
    readProcess' exe _ args env =
        readCreateProcess (Process.proc exe args){env} ""

data SubcommandAnn
    = AliasName
    | SubcommandName
    | Description

describeSubcommands
    :: [SubcommandDescription String String]
    -> ListOptions
    -> AppNames
    -> Config
    -> IO ()
describeSubcommands internalSubcommands opts appNames config = do
    descriptions <- getSubcommandDescriptions internalSubcommands
        describeSubcommand describeAlias opts appNames config
    hPutSubcommandDoc config opts (Pretty.vsep descriptions <> Pretty.line)
  where
    hPutSubcommandDoc :: Config -> ListOptions -> Doc SubcommandAnn -> IO ()
    hPutSubcommandDoc Config{colourOutput} ListOptions{output} =
        hPutDoc defaultLayoutOptions toAnsi colourOutput output
      where
        toAnsi :: SubcommandAnn -> Pretty.AnsiStyle
        toAnsi = \case
            AliasName -> Pretty.color Pretty.Magenta
            SubcommandName -> Pretty.color Pretty.Magenta
            Description -> Pretty.colorDull Pretty.White

    describeAlias :: Alias -> Doc SubcommandAnn
    describeAlias Alias{alias, command, description} =
        Pretty.annotate AliasName (fromString alias)
        <+> (   Pretty.annotate Description "[alias for '"
            <>  Pretty.annotate SubcommandName (fromString command)
            <>  Pretty.annotate Description ("']" <+> pretty description)
            )

    describeSubcommand
        :: SubcommandDescription  String String
        -> Doc SubcommandAnn
    describeSubcommand SubcommandDescription{name, description} =
        Pretty.annotate SubcommandName (fromString name)
        <+> Pretty.annotate Description (pretty description)

data TreeOptions = TreeOptions
    { delimiter :: Char
    , showDescription :: Bool
    }
  deriving stock (Generic, Show)

data TreeAnn
    = NodeName
    | LeafNodeName
    | NodeDescription

treeAnnToAnsi :: TreeAnn -> Pretty.AnsiStyle
treeAnnToAnsi = \case
    LeafNodeName -> Pretty.color Pretty.Magenta
    NodeName -> Pretty.color Pretty.Blue
    NodeDescription -> Pretty.colorDull Pretty.White

subcommandTree
    :: [SubcommandDescription String String]
    -> ListOptions
    -> AppNames
    -> Config
    -> IO ()
subcommandTree internalSubcommands opts@ListOptions{treeOptions} appNames
  config = do
    descriptions <- getSubcommandDescriptions internalSubcommands
        convertSubcommand convertAlias opts appNames config
    hPutTreeDoc config opts
        (commandTree treeOptions descriptions <> Pretty.line)
  where
    convertAlias :: Alias -> SubcommandDescription Text (Maybe (Doc TreeAnn))
    convertAlias Alias{alias, command, description} = SubcommandDescription
        { name = fromString alias
        , description = description >>= \s ->
            (   Pretty.annotate NodeDescription "[alias for '"
            <>  Pretty.annotate LeafNodeName (fromString command)
            <>  Pretty.annotate NodeDescription ("']" <+> fromString s)
            ) <$ guard (not (List.null s))
        }

    convertSubcommand
        :: SubcommandDescription String String
        -> SubcommandDescription Text (Maybe (Doc TreeAnn))
    convertSubcommand SubcommandDescription{..} = SubcommandDescription
        { name = fromString name
        , description =
            Pretty.annotate NodeDescription (fromString description)
                <$ guard (not (List.null description))
        }

    hPutTreeDoc :: Config -> ListOptions -> Doc TreeAnn -> IO ()
    hPutTreeDoc Config{colourOutput} ListOptions{output} =
        hPutDoc defaultLayoutOptions treeAnnToAnsi colourOutput output

commandTree
    :: TreeOptions
    -> [SubcommandDescription Text (Maybe (Doc TreeAnn))]
    -> Doc TreeAnn
commandTree TreeOptions{..} commands =
    Pretty.vsep $ List.drop 1 (draw (Node "" forest))
  where
    uniqueCommands :: [SubcommandDescription Text (Maybe (Doc TreeAnn))]
    uniqueCommands = List.nubBy ((==) `on` name) commands

    names :: [SubcommandDescription [Text] (Maybe (Doc TreeAnn))]
    names =
        bimap (Text.split (== delimiter)) (guard showDescription *>)
        <$> uniqueCommands

    groupNames
        :: [SubcommandDescription [Text] (Maybe (Doc TreeAnn))]
        -> [[SubcommandDescription [Text] (Maybe (Doc TreeAnn))]]
    groupNames =
        List.groupBy ((==) `on` (headMay . name))
            . List.sortBy (compare `on` (headMay . name))

    forest :: Forest (Doc TreeAnn)
    forest = (`unfoldForest` groupNames names) \case
        -- n :: Text
        -- r :: [Text]
        -- description :: Maybe (Doc TreeAnn)
        -- ns :: [SubcommandDescription [Text] (Maybe (Doc TreeAnn))]
        SubcommandDescription{name = n : r, description} : ns ->
            let rs :: [[SubcommandDescription [Text] (Maybe (Doc TreeAnn))]]
                rs = groupNames
                    $ SubcommandDescription{name = r, description}
                    : fmap (first (List.drop 1)) ns

                isLeaf :: Bool
                isLeaf = [[]] `List.elem` fmap (fmap name) rs

                ann :: Doc TreeAnn -> Doc TreeAnn
                ann = Pretty.annotate if isLeaf
                    then LeafNodeName
                    else NodeName

             in ( ann (pretty n) <> case description of
                    Just d | isLeaf ->
                        Pretty.space <> d
                    _ ->
                        mempty
                , List.filter ((/= [[]]) . fmap name) rs
                )

        _ -> (mempty, []) -- This should not happen.

    draw :: Tree (Doc ann) -> [Doc ann]
    draw (Node x ts0) = x : drawSubTrees ts0
      where
        drawSubTrees = \case
            [] ->
                []
            [t] ->
                shift "└── " "    " (draw t)
            t : ts ->
                shift "├── " "│   " (draw t) <> drawSubTrees ts

        shift first' other = List.zipWith (<>) (first' : List.repeat other)
