{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
-- |
-- Module:      CommandWrapper.Internal.Subcommand.Completion
-- Description: TODO: Module synopsis
-- Copyright:   (c) 2018-2019 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions; POSIX.
--
-- TODO: Module description.
module CommandWrapper.Internal.Subcommand.Completion
    ( CompletionMode(..)
    , completion
    , completionSubcommandHelp
    )
  where

import Prelude ((+), (-), fromIntegral)

import Control.Applicative ((<*>), (<|>), many, optional, pure)
import Control.Monad ((>>=), join)
import Data.Bool (Bool(False), otherwise)
import qualified Data.Char as Char (isDigit, toLower)
import Data.Either (Either(Left, Right))
import Data.Eq ((==))
import Data.Foldable (all, asum, forM_, length, mapM_, null)
import Data.Function (($), (.), const, id)
import Data.Functor (Functor, (<$>), (<&>), fmap)
import qualified Data.List as List
    ( break
    , elem
    , filter
    , isPrefixOf
    , lookup
    , nub
    , take
    , takeWhile
    )
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Monoid (Endo(Endo, appEndo), mconcat, mempty)
import Data.Semigroup ((<>))
import Data.String (String, fromString)
import Data.Tuple (fst)
import Data.Word (Word)
import GHC.Generics (Generic)
import System.Exit (ExitCode(ExitFailure), exitWith)
import System.IO (IO, putStrLn, stderr, stdout)
import Text.Read (readMaybe)
import Text.Show (Show)

import Data.Monoid.Endo (mapEndo)
import Data.Monoid.Endo.Fold (dualFoldEndo, foldEndo)
import Data.Text (Text)
import qualified Data.Text.IO as Text (putStrLn)
import Data.Text.Prettyprint.Doc ((<+>), pretty)
import qualified Data.Text.Prettyprint.Doc as Pretty
    ( Doc
    , brackets
    , squotes
    , vsep
    )
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Pretty (AnsiStyle)
import qualified Data.Text.Prettyprint.Doc.Util as Pretty (reflow)
import qualified Dhall
import qualified Dhall.TH (staticDhallExpression)
import qualified Mainplate (applySimpleDefaults)
import qualified Options.Applicative as Options
    ( Parser
    , defaultPrefs
    , eitherReader
    , flag
    , info
    , long
    , metavar
    , option
    , short
    , strOption
    )
import Safe (atMay, headMay, lastMay)
import Text.Fuzzy as Fuzzy (Fuzzy)
import qualified Text.Fuzzy as Fuzzy (Fuzzy(original), filter)

import qualified CommandWrapper.Config.Global as Global (Config(..))
import CommandWrapper.Environment (AppNames(AppNames, exePath, usedName))
import qualified CommandWrapper.External as External (findSubcommands)
import CommandWrapper.Internal.Subcommand.Help
    ( globalOptionsHelp
    , helpOptions
    , longOption
    , metavar
    , optionDescription
    , section
    , toolsetCommand
    , usageSection
    , value
    )
import CommandWrapper.Internal.Utils (runMain)
import CommandWrapper.Message (Result, defaultLayoutOptions, errorMsg, message)
import CommandWrapper.Options.Alias (Alias(alias))
import qualified CommandWrapper.Options.ColourOutput as ColourOutput
    ( ColourOutput(Auto)
    )
import qualified CommandWrapper.Options.Optparse as Options
    ( internalSubcommandParse
    , splitArguments
    )
import qualified CommandWrapper.Options.Shell as Options


data Query = Query
    { what :: WhatToQuery

    , pattern :: String
    -- ^ TODO: This conflates pattern not present and empty pattern.  Maybe we
    -- should support multiple patterns, in which case this will nicely unite
    -- with 'CompletionOptions'.

    , caseSensitive :: Bool
    -- ^ Be case sensitive when pattern matching values.
    --
    -- * @False@: don't be case sensitive when pattern matching.
    -- * @True@: be case sensitive when pattern matching.
    }
  deriving (Generic, Show)
    -- TODO: Extend this data type to be able to list various combinations of
    -- following groups:
    --
    -- - External subcommands
    -- - Internal subcommands
    -- - Aliases
    -- - Global options

data WhatToQuery
    = QuerySubcommands
    | QuerySubcommandAliases
    | QueryVerbosityValues
    | QueryColourValues
    | QuerySupportedShells
  deriving (Generic, Show)

defQueryOptions :: Query
defQueryOptions = Query
    { what = QuerySubcommands
    , pattern = ""
    , caseSensitive = False
    }

data CompletionMode a
    = CompletionMode CompletionOptions a
    | ScriptMode ScriptOptions a
    | QueryMode Query a
    | HelpMode a
  deriving stock (Functor, Generic, Show)

data CompletionOptions = CompletionOptions
    { words :: [String]
    , index :: Maybe Word
    , shell :: Options.Shell
    , subcommand :: Maybe String
    }
  deriving stock (Generic, Show)

data ScriptOptions = ScriptOptions
    { aliases :: [String]
    , shell :: Options.Shell
    }
  deriving stock (Generic, Show)

-- | We'll change this data type to:
-- @
-- newtype MkCompletionScript = MkCompletionScript
--     { mkCompletionScript :: 'Options.Shell' -> Text -> Text -> Text
--     }
-- @
newtype MkCompletionScript = MkCompletionScript
    { mkCompletionScript :: Text -> Text -> Text -> Scripts
    }
  deriving stock (Generic)

-- We'll get rid of this type once Dhall function is redefined to accept sum
-- type representing shell variant.
newtype Scripts = Scripts
    { bash :: Text
    }
  deriving stock (Generic, Show)
  deriving anyclass (Dhall.Interpret)

instance Options.HasShell CompletionOptions where
    updateShell =
        mapEndo \f opts@CompletionOptions{shell} ->
            (opts :: CompletionOptions){shell = f shell}

instance Options.HasShell ScriptOptions where
    updateShell = mapEndo \f ScriptOptions{aliases, shell} ->
        ScriptOptions{aliases, shell = f shell}

instance Options.HasShell (CompletionMode cfg) where
    updateShell f = Endo \case
        CompletionMode opts cfg ->
            CompletionMode (Options.updateShell f `appEndo` opts) cfg

        ScriptMode opts cfg ->
            ScriptMode (Options.updateShell f `appEndo` opts) cfg

        mode ->
            mode

defScriptOptions :: ScriptOptions
defScriptOptions = ScriptOptions
    { shell = Options.Bash
    , aliases = []
    }

completion :: AppNames -> [String] -> Global.Config -> IO ()
completion appNames options config =
    runMain (parseOptions appNames config options) defaults \case
        -- TODO:
        --
        -- - This subcommand will need access to global parser definition to
        --   provide completion for global options.
        --
        -- - When completing subcommand names we need to find possible
        --   completions in:
        --
        --     - All possible external and internal subcommands.
        --     - All aliases defined by `command-wrapper` and `TOOLSET` (if not
        --       invoking `command-wrapper` directly).
        --
        -- - When completing option/argument of a subcommand we need to execute
        --   the subcommand with bash completion options passed to it. This
        --   will require us to extend `SUBCOMMAND_PROTOCOL.md`. Should we rely
        --   on `optparse-applicative` for this?
        CompletionMode CompletionOptions{..} () -> do
            completions <- case subcommand of
                Nothing -> do
                    let pat = fromMaybe ""
                            $ maybe (lastMay words)
                                (atMay words . fromIntegral) index

                    let subcmds = findSubcommands appNames config pat

                    case headMay pat of
                        Nothing  ->
                            (findOptions config ('-' : pat) <>) <$> subcmds
                        Just '-' ->
                            pure (findOptions config pat)
                        _ ->
                            subcmds

                Just subcommand' ->
                    let index' = case length words of
                            0 -> 0
                            n -> fromIntegral n - 1
                     in subcommandCompletion appNames config shell
                            (fromMaybe index' index) words subcommand'

            mapM_ putStrLn completions

        -- TODO:
        --
        -- - Completion script should be configurable.
        --
        -- - By default it should be printed to `stdout`, but we should support
        --   writting it into a file without needing to redirect `stdout`.
        ScriptMode ScriptOptions{shell = _, aliases} () -> do
            let mkCompletionScriptExpr =
                    $(Dhall.TH.staticDhallExpression
                        "./dhall/CommandWrapper/completion.dhall"
                    )

                AppNames{exePath, usedName} = appNames

            case MkCompletionScript <$> Dhall.extract Dhall.auto mkCompletionScriptExpr of
                Nothing -> do
                    -- TODO: Figure out how to handle this better.
                    let Global.Config{colourOutput, verbosity} = config
                        colour = fromMaybe ColourOutput.Auto colourOutput
                        subcommand = pretty (usedName <> " completion:")
                    errorMsg subcommand verbosity colour stderr
                        "Failed to generate completion script."
                    -- TODO: This is probably not the best exit code.
                    exitWith (ExitFailure 1)

                Just (MkCompletionScript mkScript) ->
                    forM_ (usedName : aliases) \name ->
                        let Scripts{bash} = mkScript
                                (fromString name :: Text)
                                (fromString usedName :: Text)
                                (fromString exePath :: Text)

                        in Text.putStrLn bash

        QueryMode query@Query{..} () -> case what of
            QuerySubcommands
              | null pattern ->
                    getSubcommands appNames config >>= mapM_ putStrLn

              | otherwise ->
                    findSubcommandsFuzzy config query
                        >>= mapM_ (putStrLn . Fuzzy.original)

            QuerySubcommandAliases
              | null pattern ->
                    mapM_ putStrLn (getAliases config)

              | otherwise ->
                    mapM_ (putStrLn . Fuzzy.original)
                        (findSubcommandAliasesFuzzy config query)

            QueryVerbosityValues
              | null pattern ->
                    mapM_ putStrLn verbosityValues

              | otherwise ->
                    mapM_ (putStrLn . Fuzzy.original)
                        ( Fuzzy.filter pattern verbosityValues "" "" id
                            caseSensitive
                        )

            QueryColourValues
              | null pattern ->
                    mapM_ putStrLn colourValues

              | otherwise ->
                    mapM_ (putStrLn . Fuzzy.original)
                        ( Fuzzy.filter pattern colourValues "" "" id
                            caseSensitive
                        )

            QuerySupportedShells
              | null pattern ->
                    mapM_ putStrLn supportedShells

              | otherwise ->
                    mapM_ (putStrLn . Fuzzy.original)
                        ( Fuzzy.filter pattern supportedShells "" "" id
                            caseSensitive
                        )

        HelpMode () ->
            let Global.Config{colourOutput, verbosity} = config
                colour = fromMaybe ColourOutput.Auto colourOutput

            in message defaultLayoutOptions verbosity colour stdout
                (completionSubcommandHelp appNames)
  where
    defaults =
        let opts = CompletionOptions
                { words = []
                , index = Nothing
                , shell = Options.Bash
                , subcommand = Nothing
                }

        in Mainplate.applySimpleDefaults (CompletionMode opts ())

    getAliases :: Global.Config -> [String]
    getAliases = List.nub . fmap alias . Global.aliases

    findSubcommandsFuzzy :: Global.Config -> Query -> IO [Fuzzy String String]
    findSubcommandsFuzzy cfg Query{pattern, caseSensitive} = do
        cmds <- getSubcommands appNames cfg
        pure (Fuzzy.filter pattern cmds "" "" id caseSensitive)

    findSubcommandAliasesFuzzy
        :: Global.Config
        -> Query
        -> [Fuzzy String String]
    findSubcommandAliasesFuzzy cfg Query{pattern, caseSensitive} =
        Fuzzy.filter pattern (getAliases cfg) "" "" id caseSensitive

    -- TODO: Generate these values instead of hard-coding them.
    verbosityValues :: [String]
    verbosityValues = ["silent", "quiet", "normal", "verbose", "annoying"]

    -- TODO: Generate these values instead of hard-coding them.
    colourValues :: [String]
    colourValues = ["always", "auto", "never"]

    -- TODO: Generate these values instead of hard-coding them.
    supportedShells :: [String]
    supportedShells = ["bash"]

    findOptions :: Global.Config -> String -> [String]
    findOptions cfg pat =
        let -- TODO: Generate these values instead of hard-coding them.
            shortOptions :: [String]
            shortOptions =
                [ "-v", "-vv", "-vvv"
                , "-s", "-q"
                , "-V"
                ]

            longOptions :: [(String, Maybe (String -> [String]))]
            longOptions =
                [ ("--color=", Just (findKeywords colourValues cfg))
                , ("--colour=", Just (findKeywords colourValues cfg))
                , ("--no-aliases", Nothing)
                , ("--no-color", Nothing)
                , ("--no-colour", Nothing)
                , ("--version", Nothing)
                , ("--verbosity=", Just (findKeywords verbosityValues cfg))
                ]

        in  case List.takeWhile (== '-') pat of
                -- Search for both, long and short option.
                "-" ->
                    (if pat == "-" then ("--" :) else id)
                    $  List.filter (pat `List.isPrefixOf`) shortOptions
                    <> List.filter (fmap Char.toLower pat `List.isPrefixOf`)
                        (fmap fst longOptions)

                -- Search for long option.
                "--" ->
                    (if pat == "--" then ("--" :) else id)
                    $ case List.break (== '=') pat of
                        (opt, '=' : pat') ->
                            case join (List.lookup (opt <> "=") longOptions) of
                                Just f -> ((opt <> "=") <>) <$> f pat'
                                _ -> []
                        _ ->
                            List.filter
                                (fmap Char.toLower pat `List.isPrefixOf`)
                                (fst <$> longOptions)

                -- No option starts with "---" or more.
                _ -> []

    findKeywords :: [String] -> Global.Config -> String -> [String]
    findKeywords keywords _cfg pat =
        List.filter (fmap Char.toLower pat `List.isPrefixOf`) keywords

subcommandCompletion
    :: AppNames
    -> Global.Config
    -> Options.Shell
    -> Word
    -- ^ Index inside arguments.
    -> [String]
    -- ^ Subcommand arguments.
    -> String
    -- ^ Subcommand name.
    -> IO [String]
subcommandCompletion appNames config _shell index words = \case
    "help" -> helpCompletion appNames config hadDashDash pat
    "config" -> pure []
    "completion" -> pure []
    "version" -> pure []
    _subcommand -> pure []
  where
    hadDashDash = "--" `List.elem` List.take (fromIntegral index + 1) words

    pat = fromMaybe "" $ atMay words (fromIntegral index)

helpCompletion :: AppNames -> Global.Config -> Bool -> String -> IO [String]
helpCompletion appNames config hadDashDash pat
  | hadDashDash = subcmds
  | null pat    = (helpOptions' <>) <$> subcmds
  | isOption    = pure $ List.filter (pat `List.isPrefixOf`) helpOptions'
  | otherwise   = subcmds
  where
    isOption = headMay pat == Just '-'
    helpOptions' = ["--help", "-h", "--"]
    subcmds = findSubcommands appNames config pat

findSubcommands :: AppNames -> Global.Config -> String -> IO [String]
findSubcommands appNames config pat =
    List.filter (fmap Char.toLower pat `List.isPrefixOf`)
        <$> getSubcommands appNames config

getSubcommands :: AppNames -> Global.Config -> IO [String]
getSubcommands appNames config = do
    extCmds <- External.findSubcommands appNames config

    let aliases = alias <$> Global.aliases config
        internalCommands = ["help", "config", "completion", "version"]

    pure (List.nub $ aliases <> internalCommands <> extCmds)

parseOptions
    :: AppNames
    -> Global.Config
    -> [String]
    -> IO (Endo (CompletionMode ()))
parseOptions appNames config arguments = do
    let (options, words) = Options.splitArguments arguments

    execParser options $ asum
        [ dualFoldEndo
            <$> scriptFlag
            <*> optional Options.shellOption
            <*> many aliasOption

        , dualFoldEndo
            <$> queryFlag
            <*> optional
                ( updateQueryOptions
                    (   subcommandsFlag
                    <|> subcommandAliasesFlag
                    <|> supportedShellsFlag
                    <|> verbosityValuesFlag
                    <|> colourValuesFlag
                    )
                )
--          <*> optional (updateQueryOptions patternArgument)

        , helpFlag

        , updateCompletionOptions words
            $ foldEndo
                <$> optional indexOption
                <*> optional Options.shellOption
                <*> optional subcommandOption
        ]
  where
    switchTo = Endo . const
    switchToScriptMode = switchTo (ScriptMode defScriptOptions ())
    switchToQueryMode = switchTo (QueryMode defQueryOptions ())
    switchToHelpMode = switchTo (HelpMode ())

    updateCompletionOptions words =
        fmap . mapEndo $ \f _ ->
            let defOpts = CompletionOptions
                    { words
                    , index = Nothing
                    , shell = Options.Bash
                    , subcommand = Nothing
                    }
            in CompletionMode (f defOpts) ()

    updateQueryOptions
        :: Options.Parser (Endo Query)
        -> Options.Parser (Endo (CompletionMode ()))
    updateQueryOptions = fmap . mapEndo $ \f -> \case
        QueryMode q cfg -> QueryMode (f q) cfg
        mode            -> mode

    scriptFlag :: Options.Parser (Endo (CompletionMode ()))
    scriptFlag = Options.flag mempty switchToScriptMode (Options.long "script")

    queryFlag :: Options.Parser (Endo (CompletionMode ()))
    queryFlag = Options.flag mempty switchToQueryMode (Options.long "query")

    subcommandsFlag :: Options.Parser (Endo Query)
    subcommandsFlag = Options.flag mempty f (Options.long "subcommands")
      where
        f = Endo \q -> q{what = QuerySubcommands}

    subcommandAliasesFlag :: Options.Parser (Endo Query)
    subcommandAliasesFlag =
        Options.flag mempty f (Options.long "subcommand-aliases")
      where
        f = Endo \q -> q{what = QuerySubcommandAliases}

    verbosityValuesFlag :: Options.Parser (Endo Query)
    verbosityValuesFlag =
        Options.flag mempty f (Options.long "verbosity-values")
      where
        f = Endo \q -> q{what = QueryVerbosityValues}

    colourValuesFlag :: Options.Parser (Endo Query)
    colourValuesFlag = Options.flag mempty f
        (Options.long "color-values" <> Options.long "colour-values")
      where
        f = Endo \q -> q{what = QueryColourValues}

    supportedShellsFlag :: Options.Parser (Endo Query)
    supportedShellsFlag =
        Options.flag mempty f (Options.long "supported-shells")
      where
        f = Endo \q -> q{what = QuerySupportedShells}

    subcommandOption :: Options.Parser (Endo CompletionOptions)
    subcommandOption =
        Options.strOption (Options.long "subcommand") <&> \subcommand ->
            Endo \q -> q{subcommand = Just subcommand}

    indexOption :: Options.Parser (Endo CompletionOptions)
    indexOption =
        Options.option parse (Options.long "index" <> Options.metavar "NUM")
      where
        parse = Options.eitherReader \s ->
            if  | null s ->
                    Right mempty

                | all Char.isDigit s ->
                    case readMaybe s of
                        Nothing ->
                            Left "Non-negative number expected"

                        index ->
                            Right . Endo $ \opts -> opts{index}

                | otherwise ->
                    Left "Non-negative number expected"

    aliasOption :: Options.Parser (Endo (CompletionMode ()))
    aliasOption =
        Options.strOption (Options.long "alias" <> Options.metavar "ALIAS")
            <&> \alias -> Endo \case
                    ScriptMode ScriptOptions{aliases, shell} cfg ->
                        ScriptMode
                            ( ScriptOptions
                                { aliases = aliases <> [alias]
                                , shell
                                }
                            )
                            cfg
                    mode ->
                        mode

    helpFlag = Options.flag mempty switchToHelpMode $ mconcat
        [ Options.short 'h'
        , Options.long "help"
        ]

    execParser options parser =
        Options.internalSubcommandParse appNames config "completion"
            Options.defaultPrefs (Options.info parser mempty) options

completionSubcommandHelp :: AppNames -> Pretty.Doc (Result Pretty.AnsiStyle)
completionSubcommandHelp AppNames{usedName} = Pretty.vsep
    [ usageSection usedName
        [ "completion"
            <+> Pretty.brackets
                ( longOption "index" <> "=" <> metavar "NUM"
                )
            <+> Pretty.brackets
                ( longOption "shell" <> "=" <> metavar "SHELL"
                )
            <+> Pretty.brackets
                ( longOption "subcommand" <> "=" <> metavar "SUBCOMMAND"
                )
            <+> value "--"
            <+> Pretty.brackets (metavar "WORD" <+> "...")

        , "completion"
            <+> longOption "script"
            <+> Pretty.brackets (longOption "shell" <> "=" <> metavar "SHELL")
            <+> Pretty.brackets
                ( longOption "alias" <> "=" <> metavar "ALIAS" <+> "..."
                )

        , "completion"
            <+> longOption "query"
            <+> Pretty.brackets
                ( longOption "subcommands"
                <> "|" <> longOption "subcommand-aliases"
                <> "|" <> longOption "supported-shells"
                <> "|" <> longOption "verbosity-values"
                <> "|" <> longOption "colo[u]r-values"
                )
--          <+> Pretty.brackets (metavar "PATTERN")

        , "completion" <+> helpOptions
        , "help completion"
        ]

    , section "Command line completion options:"
        [ optionDescription ["--index=NUM"]
            [ Pretty.reflow "Position of a", metavar "WORD"
            , Pretty.reflow "for which we want completion. In Bash this is the"
            , Pretty.reflow "value of", value "COMP_CWORD", "variable."
            ]

        , optionDescription ["--shell=SHELL"]
            [ Pretty.reflow "Provide completion for", metavar "SHELL" <> "."
            , Pretty.reflow "Currently only supported value is"
            , value "bash" <> "."
            ]

        , optionDescription ["--subcommand=SUBCOMMAND"]
            [ Pretty.reflow "Do command line completion for a"
            , metavar "SUBCOMMAND", "instead."
            ]

        , optionDescription ["WORD"]
            [ metavar "WORD" <> "s", Pretty.reflow "to complete. In Bash these"
            , Pretty.reflow "are the elements of", value "COMP_WORDS", "array."
            ]
        ]

    , section "Completion script options:"
        [ optionDescription ["--script"]
            [ Pretty.reflow "Generate completion script suitable for sourcing"
            , Pretty.reflow "in your shell's *rc file."
            ]

        , optionDescription ["--shell=SHELL"]
            [ Pretty.reflow "Generate completion script for"
            , metavar "SHELL" <> "."
            , Pretty.reflow "Currently only supported value is"
            , value "bash" <> "."
            ]

        , optionDescription ["--alias=ALIAS"]
            [ metavar "ALIAS"
            , Pretty.reflow "under which Command Wrapper toolset is also known."
            , Pretty.reflow "This is usually name of a shell alias, e.g."
            , Pretty.squotes (value "alias ts=toolset")
            , "where", value "ts", Pretty.reflow "is an", metavar "ALIAS"
            , Pretty.reflow "for which we want command line completion to work"
            , Pretty.reflow "as well."
            ]
        ]

    , section "Query options:"
        [ optionDescription ["--query"]
            [ Pretty.reflow "Query command line interface."
            , Pretty.reflow "Useful for editor/IDE integration."
            ]

        , optionDescription ["--subcommands"]
            [ Pretty.reflow "Query all available subcommands. This includes\
                \ internal subcommands, external subcommands, and subcommand\
                \ aliases."
            ]

--      , optionDescription ["--fail-when-no-match"]
--          [ Pretty.reflow "Terminate with exit code 3 if no match was found."
--          ]

--      , optionDescription ["--algorithm=ALGORITHM"]
--          [ Pretty.reflow "Specify which pattern matching algorithm to use"
--          , "when", metavar "PATTERN"
--          , Pretty.reflow "is provided. Possible values are:"
--          , value "prefix" <> ",", value "fuzzy" <> ",", "and"
--          , value "equality."
--          ]

--      , optionDescription ["--external-subcommands"]
--          [ Pretty.reflow "Query available external subcommands."
--          ]

--      , optionDescription ["--internal-subcommands"]
--          [ Pretty.reflow "Query available internal subcommands."
--          ]

        , optionDescription ["--subcommand-aliases"]
            [ Pretty.reflow "Query available subcommand aliases."
            ]

        , optionDescription ["--supported-shells"]
            [ Pretty.reflow "Query shells supported by command line completion."
            ]

        , optionDescription ["--verbosity-values"]
            [ Pretty.reflow "Query possible"
            , metavar "VERBOSITY"
            , Pretty.reflow "values. These can be set using global"
            , longOption "verbosity" <> "=" <> metavar "VERBOSITY"
            , Pretty.reflow "option, or are passed down to subcommands via"
            , metavar "COMMAND_WRAPPER_VERBOSITY"
            , Pretty.reflow "environment variable."
            ]

        , optionDescription ["--colo[u]r-values"]
            [ Pretty.reflow "Query possible"
            , metavar "WHEN"
            , Pretty.reflow "colour output values. These can be set using"
            , "global", longOption "colo[u]r" <> "=" <> metavar "WHEN"
            , Pretty.reflow "option, or are passed down to subcommands via"
            , metavar "COMMAND_WRAPPER_COLOUR"
            , Pretty.reflow "environment variable."
            ]

--      , optionDescription ["PATTERN"]
--          [ Pretty.reflow "TODO: Document"
--          ]
        ]

    , section "Other options:"
        [ optionDescription ["--help", "-h"]
            [ Pretty.reflow "Print this help and exit. Same as"
            , Pretty.squotes (toolsetCommand usedName "help completion") <> "."
            ]

        , globalOptionsHelp usedName
        ]

    , ""
    ]
