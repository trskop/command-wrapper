{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module:      CommandWrapper.Internal.Subcommand.Completion
-- Description: Implementation of internal subcommand that provides command
--              line completion and support for IDE-like functionality.
-- Copyright:   (c) 2018-2019 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions; POSIX.
--
-- Implementation of internal subcommand that provides command line completion
-- and support for IDE-like functionality.
module CommandWrapper.Internal.Subcommand.Completion
    ( CompletionMode(..)
    , completion
    , completionSubcommandHelp
    )
  where

import Prelude ((+), (-), fromIntegral)

import Control.Applicative ((<*>), (<|>), many, optional, pure, some)
import Control.Monad ((>>=), join, when)
import Data.Bool (Bool(False), otherwise)
import qualified Data.Char as Char (isDigit, toLower)
import Data.Either (Either(Left, Right))
import Data.Eq ((/=), (==))
import Data.Foldable (all, asum, forM_, length, mapM_, null)
import Data.Function (($), (.), const, id)
import Data.Functor (Functor, (<$>), (<&>), fmap)
import qualified Data.List as List
    ( break
    , drop
    , elem
    , filter
    , isPrefixOf
    , lookup
    , nub
    , take
    , takeWhile
    , unlines
    )
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (Endo(Endo, appEndo), mconcat, mempty)
import Data.Ord ((>))
import Data.Semigroup ((<>))
import Data.String (String, fromString)
import Data.Tuple (fst)
import Data.Word (Word)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import System.Exit (ExitCode(ExitFailure, ExitSuccess), exitWith)
import System.IO (IO, putStrLn, stderr, stdout, writeFile)
import Text.Read (readMaybe)
import Text.Show (Show, show)

import qualified Data.ByteString as ByteString (putStr, writeFile)
import Data.FileEmbed (embedFile)
import Data.Generics.Product.Typed (typed)
import Data.Monoid.Endo (mapEndo)
import Data.Monoid.Endo.Fold (dualFoldEndo, foldEndo)
import Data.Output
    ( HasOutput(Output)
    , OutputFile(OutputFile)
    , OutputHandle(OutputHandle, OutputNotHandle)
    , OutputStdoutOrFile
    , pattern OutputStdoutOnly
    , setOutput
    )
import qualified Data.Output as Output (HasOutput(output))
import Data.Text (Text)
import qualified Data.Text as Text (unlines, unpack)
import qualified Data.Text.IO as Text (putStrLn, writeFile)
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
import qualified Options.Applicative.Standard as Options (outputOption)
import Safe (atMay, headMay)
import System.Process (CreateProcess(env), proc, readCreateProcessWithExitCode)
import Text.Fuzzy as Fuzzy (Fuzzy)
import qualified Text.Fuzzy as Fuzzy (Fuzzy(original), filter)

import qualified CommandWrapper.Config.Global as Global (Config(..))
import CommandWrapper.Environment (AppNames(AppNames, exePath, usedName))
import qualified CommandWrapper.External as External
    ( executeCommand
    , executeCommandWith
    , findSubcommands
    )
import CommandWrapper.Internal.Subcommand.Config (configCompletion)
import CommandWrapper.Internal.Subcommand.Help
    ( globalOptionsHelp
    , helpOptions
    , longOption
    , longOptionWithArgument
    , metavar
    , optionDescription
    , section
    , toolsetCommand
    , usageSection
    , value
    )
import CommandWrapper.Internal.Subcommand.Version (versionCompletion)
import CommandWrapper.Internal.Utils (runMain)
import CommandWrapper.Message (Result, defaultLayoutOptions, errorMsg, message)
import CommandWrapper.Options.Alias (Alias(alias))
import qualified CommandWrapper.Options.ColourOutput as ColourOutput
    ( ColourOutput(Auto)
    )
import qualified CommandWrapper.Options.Alias as Options (applyAlias)
import qualified CommandWrapper.Options.Optparse as Options
    ( internalSubcommandParse
    , splitArguments
    , splitArguments'
    )
import qualified CommandWrapper.Options.Shell as Options


data Query = Query
    { what :: WhatToQuery

    , patternToMatch :: String
    -- ^ TODO: This conflates pattern not present and empty pattern.  Maybe we
    -- should support multiple patterns, in which case this will nicely unite
    -- with 'CompletionOptions'.

    , caseSensitive :: Bool
    -- ^ Be case sensitive when pattern matching values.
    --
    -- * @False@: don't be case sensitive when pattern matching.
    -- * @True@: be case sensitive when pattern matching.
    , output :: OutputStdoutOrFile
    }
  deriving (Generic, Show)
    -- TODO: Extend this data type to be able to list various combinations of
    -- following groups:
    --
    -- - External subcommands
    -- - Internal subcommands
    -- - Aliases
    -- - Global options

instance HasOutput Query where
    type Output Query = OutputStdoutOrFile
    output = typed

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
    , patternToMatch = ""
    , caseSensitive = False
    , output = OutputStdoutOnly
    }

data CompletionMode a
    = CompletionMode CompletionOptions a
    | ScriptMode ScriptOptions a
    | LibraryMode LibraryOptions a
    | QueryMode Query a
    | HelpMode a
  deriving stock (Functor, Generic, Show)

updateOutput :: OutputStdoutOrFile -> Endo (CompletionMode b)
updateOutput o = Endo \case
    CompletionMode opts a -> CompletionMode (setOutput o opts) a
    ScriptMode opts a -> ScriptMode (setOutput o opts) a
    LibraryMode opts a -> LibraryMode (setOutput o opts) a
    QueryMode opts a -> QueryMode (setOutput o opts) a
    HelpMode a -> HelpMode a

data CompletionOptions = CompletionOptions
    { words :: [String]
    , index :: Maybe Word
    , shell :: Options.Shell
    , subcommand :: Maybe String
    , output :: OutputStdoutOrFile
    }
  deriving stock (Generic, Show)

instance HasOutput CompletionOptions where
    type Output CompletionOptions = OutputStdoutOrFile
    output = typed

data ScriptOptions = ScriptOptions
    { aliases :: [String]
    , shell :: Options.Shell
    , subcommand :: Maybe String
    , output :: OutputStdoutOrFile
    }
  deriving stock (Generic, Show)

instance HasOutput ScriptOptions where
    type Output ScriptOptions = OutputStdoutOrFile
    output = typed

data LibraryOptions = LibraryOptions
    { shell :: Options.Shell
    , output :: OutputStdoutOrFile
    }
  deriving stock (Generic, Show)

instance HasOutput LibraryOptions where
    type Output LibraryOptions = OutputStdoutOrFile
    output = typed

-- | We'll change this data type to:
-- @
-- newtype MkCompletionScript = MkCompletionScript
--     { mkCompletionScript :: 'Options.Shell' -> Text -> Text -> Text
--     }
-- @
newtype MkCompletionScript = MkCompletionScript
    { mkCompletionScript :: Text -> Text -> Text -> Maybe Text -> Scripts
    }
  deriving stock (Generic)

-- We'll get rid of this type once Dhall function is redefined to accept sum
-- type representing shell variant.
data Scripts = Scripts
    { bash :: Text
    , fish :: Text
    , zsh :: Text
    }
  deriving stock (Generic, Show)
  deriving anyclass (Dhall.Interpret)

type CompletionInfo = Options.Shell -> Natural -> [Text] -> [Text]

instance Options.HasShell CompletionOptions where
    updateShell =
        mapEndo \f opts@CompletionOptions{shell} ->
            (opts :: CompletionOptions){shell = f shell}

instance Options.HasShell ScriptOptions where
    updateShell = mapEndo \f opts@ScriptOptions{shell} ->
        (opts :: ScriptOptions){shell = f shell}

instance Options.HasShell LibraryOptions where
    updateShell = mapEndo \f opts@LibraryOptions{shell} ->
        (opts :: LibraryOptions){shell = f shell}

instance Options.HasShell (CompletionMode cfg) where
    updateShell f = Endo \case
        CompletionMode opts cfg ->
            CompletionMode (Options.updateShell f `appEndo` opts) cfg

        ScriptMode opts cfg ->
            ScriptMode (Options.updateShell f `appEndo` opts) cfg

        LibraryMode opts cfg ->
            LibraryMode (Options.updateShell f `appEndo` opts) cfg

        mode ->
            mode

class HasSubcommand a where
    updateSubcommand :: Endo (Maybe String) -> Endo a

instance HasSubcommand CompletionOptions where
    updateSubcommand =
        mapEndo \f opts@CompletionOptions{subcommand} ->
            (opts :: CompletionOptions){subcommand = f subcommand}

instance HasSubcommand ScriptOptions where
    updateSubcommand =
        mapEndo \f opts@ScriptOptions{subcommand} ->
            (opts :: ScriptOptions){subcommand = f subcommand}

instance HasSubcommand (CompletionMode cfg) where
    updateSubcommand f = Endo \case
        CompletionMode opts cfg ->
            CompletionMode (updateSubcommand f `appEndo` opts) cfg

        ScriptMode opts cfg ->
            ScriptMode (updateSubcommand f `appEndo` opts) cfg

        mode ->
            mode

defScriptOptions :: ScriptOptions
defScriptOptions = ScriptOptions
    { shell = Options.Bash
    , subcommand = Nothing
    , aliases = []
    , output = OutputStdoutOnly
    }

defLibraryOptions :: LibraryOptions
defLibraryOptions = LibraryOptions
    { shell = Options.Bash
    , output = OutputStdoutOnly
    }

completion :: AppNames -> [String] -> Global.Config -> IO ()
completion appNames options config =
    runMain (parseOptions appNames config options) defaults \case
        CompletionMode opts@CompletionOptions{output} () ->
            getCompletions appNames config opts >>= outputStringLines output

        -- TODO:
        --
        -- - Completion script should be configurable.
        ScriptMode ScriptOptions{shell, aliases, subcommand, output} () -> do
            let mkCompletionScriptExpr =
                    $(Dhall.TH.staticDhallExpression
                        "./dhall/completion.dhall"
                    )

                AppNames{exePath, usedName} = appNames

            case MkCompletionScript <$> Dhall.extract Dhall.auto mkCompletionScriptExpr of
                Nothing -> do
                    -- TODO: Figure out how to handle this better.
                    let Global.Config{colourOutput, verbosity} = config
                        colour = fromMaybe ColourOutput.Auto colourOutput

                        subcommand' :: forall ann. Pretty.Doc ann
                        subcommand' = pretty (usedName <> " completion")

                    errorMsg subcommand' verbosity colour stderr
                        "Failed to generate completion script."
                    -- TODO: This is probably not the best exit code.
                    exitWith (ExitFailure 1)

                -- TODO:
                --
                -- - Aliases should be passed to the Dhall expression so that
                --   generated script can be more optimal.  It will also reduce
                --   complexity of this code.
                --
                -- - There's a lot of duplication in here.
                --
                -- - Maybe we should consider passing Shell to the Dhall
                --   function instead of expecting multiple results.
                Just (MkCompletionScript mkScript) -> case subcommand of
                    Nothing ->
                        forM_ (usedName : aliases) \name ->
                            let Scripts{..} = mkScript
                                    (fromString name :: Text)
                                    (fromString usedName :: Text)
                                    (fromString exePath :: Text)
                                    Nothing
                             in outputTextLines output $ pure case shell of
                                    Options.Bash -> bash
                                    Options.Fish -> fish
                                    Options.Zsh  -> zsh

                    Just subcmd ->
                        forM_ aliases \name ->
                            let Scripts{..} = mkScript
                                    (fromString name :: Text)
                                    (fromString usedName :: Text)
                                    (fromString exePath :: Text)
                                    (Just (fromString subcmd :: Text))
                             in outputTextLines output $ pure case shell of
                                    Options.Bash -> bash
                                    Options.Fish -> fish
                                    Options.Zsh  -> zsh

        LibraryMode LibraryOptions{shell, output} () -> case shell of
            -- TODO: We should consider piping the output to a pager or similar
            -- tool when the stdout is attached to a terminal.  For example
            -- `bat` would be a great choice if it is installed.
            Options.Bash ->
                let lib = $(embedFile "bash/lib.sh")
                 in case output of
                        OutputHandle _ ->
                            ByteString.putStr lib

                        OutputNotHandle (OutputFile fn) ->
                            ByteString.writeFile fn lib

            _    -> do
                let Global.Config{colourOutput, verbosity} = config
                    colour = fromMaybe ColourOutput.Auto colourOutput

                    subcommand :: forall ann. Pretty.Doc ann
                    subcommand = pretty (usedName appNames <> " completion")

                errorMsg subcommand verbosity colour stderr
                    $ fromString (show shell) <> ": Unsupported SHELL value."
                exitWith (ExitFailure 125)

        QueryMode query@Query{..} () -> case what of
            QuerySubcommands
              | null patternToMatch ->
                    getSubcommands appNames config >>= outputStringLines output

              | otherwise ->
                    findSubcommandsFuzzy config query
                        >>= outputStringLines output . fmap Fuzzy.original

            QuerySubcommandAliases
              | null patternToMatch ->
                    outputStringLines output (getAliases config)

              | otherwise ->
                    outputStringLines output $ fmap Fuzzy.original
                        (findSubcommandAliasesFuzzy config query)

            QueryVerbosityValues
              | null patternToMatch ->
                    outputStringLines output verbosityValues

              | otherwise ->
                    outputStringLines output $ fmap Fuzzy.original
                        ( Fuzzy.filter patternToMatch verbosityValues "" "" id
                            caseSensitive
                        )

            QueryColourValues
              | null patternToMatch ->
                    outputStringLines output colourValues

              | otherwise ->
                    outputStringLines output $ fmap Fuzzy.original
                        ( Fuzzy.filter patternToMatch colourValues "" "" id
                            caseSensitive
                        )

            QuerySupportedShells
              | null patternToMatch ->
                    outputStringLines output supportedShells

              | otherwise ->
                    outputStringLines output $ fmap Fuzzy.original
                        ( Fuzzy.filter patternToMatch supportedShells "" "" id
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
                , output = OutputStdoutOnly
                }

        in Mainplate.applySimpleDefaults (CompletionMode opts ())

    getAliases :: Global.Config -> [String]
    getAliases = List.nub . fmap alias . Global.aliases

    findSubcommandsFuzzy :: Global.Config -> Query -> IO [Fuzzy String String]
    findSubcommandsFuzzy cfg Query{patternToMatch, caseSensitive} = do
        cmds <- getSubcommands appNames cfg
        pure (Fuzzy.filter patternToMatch cmds "" "" id caseSensitive)

    findSubcommandAliasesFuzzy
        :: Global.Config
        -> Query
        -> [Fuzzy String String]
    findSubcommandAliasesFuzzy cfg Query{patternToMatch, caseSensitive} =
        Fuzzy.filter patternToMatch (getAliases cfg) "" "" id caseSensitive

    outputStringLines :: OutputStdoutOrFile -> [String] -> IO ()
    outputStringLines = \case
        OutputHandle _ -> mapM_ putStrLn
        OutputNotHandle (OutputFile fn) -> writeFile fn . List.unlines

    outputTextLines :: OutputStdoutOrFile -> [Text] -> IO ()
    outputTextLines = \case
        OutputHandle _ -> mapM_ Text.putStrLn
        OutputNotHandle (OutputFile fn) -> Text.writeFile fn . Text.unlines

-- TODO: Generate these values instead of hard-coding them.
verbosityValues :: [String]
verbosityValues = ["silent", "quiet", "normal", "verbose", "annoying"]

-- TODO: Generate these values instead of hard-coding them.
colourValues :: [String]
colourValues = ["always", "auto", "never"]

-- TODO: Generate these values instead of hard-coding them.
supportedShells :: [String]
supportedShells = ["bash", "fish"{-, "zsh"-}]

matchGlobalOptions :: String -> [String]
matchGlobalOptions pat =
    let -- TODO: Generate these values instead of hard-coding them.
        shortOptions :: [String]
        shortOptions =
            [ "-v", "-vv", "-vvv"
            , "-s", "-q"
            , "-V"
            ]

        longOptions :: [(String, Maybe (String -> [String]))]
        longOptions =
            [ ("--color=", Just (matchKeywords colourValues))
            , ("--colour=", Just (matchKeywords colourValues))
            , ("--no-aliases", Nothing)
            , ("--no-color", Nothing)
            , ("--no-colour", Nothing)
            , ("--version", Nothing)
            , ("--verbosity=", Just (matchKeywords verbosityValues))
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
  where
    matchKeywords :: [String] -> String -> [String]
    matchKeywords keywords p =
        List.filter (fmap Char.toLower p `List.isPrefixOf`) keywords

-- TODO:
--
-- - This will need access to global parser definition to provide completion
--   for global options without the need for hardcoded values.
getCompletions :: AppNames -> Global.Config -> CompletionOptions -> IO [String]
getCompletions appNames config CompletionOptions{..} = case subcommand of
    Nothing -> do
        let (_globalOptions, n, subcommandAndItsArguments) =
                Options.splitArguments' (List.drop 1 words)

            indexPointsBeyondSubcommandName = index' > (n + 1)

        case headMay subcommandAndItsArguments of
            Nothing ->
                globalCompletion

            Just subcommandName
              | indexPointsBeyondSubcommandName ->
                    let indexOfSubcommandArgument = index' - n - 2

                        subcommandArguments =
                            List.drop 1 subcommandAndItsArguments

                     in subcommandCompletion' indexOfSubcommandArgument
                            subcommandArguments subcommandName

              | otherwise ->
                    globalCompletion

    Just subcommandName ->
        subcommandCompletion' index' words subcommandName
  where
    index' = (`fromMaybe` index) case length words of
        0 -> 0
        n -> fromIntegral n - 1

    subcommandCompletion' idx subcommandArgument subcommandName =
        let -- TODO: Figure out how to take arguments into account when
            -- applying aliases.
            (realSubcommandName, _) =
                Options.applyAlias (Global.aliases config)
                    subcommandName []

         in subcommandCompletion appNames config shell idx subcommandArgument
                subcommandName realSubcommandName

    globalCompletion =
        let pat = fromMaybe "" $ atMay words (fromIntegral index')
            subcommands = findSubcommands appNames config pat

         in case headMay pat of
                Nothing  ->
                    (matchGlobalOptions ('-' : pat) <>) <$> subcommands
                Just '-' ->
                    pure (matchGlobalOptions pat)
                _ ->
                    subcommands

-- | Complete subcommand options\/arguments.  For external commands we call the
-- binary and use protocol defined in `command-wrapper-subcommand-protocol(7)`.
subcommandCompletion
    :: AppNames
    -> Global.Config
    -> Options.Shell
    -> Word
    -- ^ Index inside arguments.
    -> [String]
    -- ^ Subcommand arguments.
    -> String
    -- ^ Name udner which the subcommand was invoked, i.e. name before
    -- resolving aliases.
    -> String
    -- ^ Subcommand name.
    -> IO [String]
subcommandCompletion appNames config shell index words _invokedAs = \case
    "help" -> helpCompletion appNames config hadDashDash pat
    "config" -> configCompletion appNames config wordsBeforePattern pat
    "completion" -> completionCompletion appNames config wordsBeforePattern pat
    "version" -> versionCompletion appNames config wordsBeforePattern pat
    subcommand -> do
        arguments <- completionInfo subcommand
            <*> pure shell
            <*> pure (fromIntegral index)
            <*> pure (fromString <$> words)

        External.executeCommand appNames config subcommand
            (Text.unpack <$> arguments)
  where
    wordsBeforePattern = List.take (fromIntegral index) words
    hadDashDash = "--" `List.elem` wordsBeforePattern
    pat = fromMaybe "" $ atMay words (fromIntegral index)

    -- Implementation of `command-wrapper-subcommand-protocol(7)` for command
    -- line completion.
    completionInfo :: String -> IO CompletionInfo
    completionInfo subcommand = do
        (exitCode, out, err)
            <- External.executeCommandWith readProcess appNames config
                subcommand ["--completion-info"]

        when (exitCode /= ExitSuccess) $ do
            let Global.Config{colourOutput, verbosity} = config
                colour = fromMaybe ColourOutput.Auto colourOutput

                subcommand' :: forall ann. Pretty.Doc ann
                subcommand' = pretty
                    $ usedName appNames <> " completion"

            errorMsg subcommand' verbosity colour stderr
                $ fromString subcommand
                <> ": Subcommand protocol violated when called with\
                    \ '--completion-info':\n"
                <> fromString err
            exitWith (ExitFailure 2)

        Dhall.input Dhall.auto (fromString out)
      where
        readProcess cmd _ args env =
            readCreateProcessWithExitCode (proc cmd args){env} ""

-- | Command line completion for internal @help@ subcommand.
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

-- | Command line completion for the @completion@ subcommand itself.
completionCompletion
    :: AppNames
    -> Global.Config
    -> [String]
    -- ^ Options and arguments before the one that we are completing.
    -> String
    -- ^ Pattern (prefix), i.e. option\/argument that we are completing.
    -> IO [String]
completionCompletion _appNames _config _wordsBeforePattern pat
    | null pat  = pure allOptions
    | otherwise = pure $ List.filter (pat `List.isPrefixOf`) allOptions
  where
    -- TODO: This implementation needs refinement:
    --
    -- - Be contextual, i.e. if there is `--query` then ignore options that do
    --   not apply.
    -- - Complete `SHELL` and `SUBCOMMAND` values.
    -- - Complete files/directories for --output=
    allOptions = List.nub
        [ "--index=", "--shell=bash", "--shell=fish", "--subcommand=", "--"

        , "--library", "--shell=bash", "--shell=fish"

        , "--query", "--subcommands", "--subcommand-aliases"
        , "--supported-shells", "--verbosity-values", "--colour-values"
        , "--color-values"

        , "--script", "--shell=bash", "--alias="

        , "--output=", "-o"

        , "--help", "-h"
        ]

-- | Lookup external and internal subcommands matching pattern (prefix).
findSubcommands
    :: AppNames
    -> Global.Config
    -> String
    -- ^ Pattern (prefix) to match subcommand name against.
    -> IO [String]
findSubcommands appNames config pat =
    List.filter (fmap Char.toLower pat `List.isPrefixOf`)
        <$> getSubcommands appNames config

-- | List all available external and internal subcommands.
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
            <*> optional outputOption
            <*> asum
                [ dualFoldEndo
                    <$> aliasOption
                    <*> many aliasOption
                    :: Options.Parser (Endo (CompletionMode ()))
                , dualFoldEndo
                    <$> subcommandOption
                    <*> some aliasOption
                , pure mempty
                ]

        , dualFoldEndo
            <$> libraryFlag
            <*> optional Options.shellOption
            <*> optional outputOption

        , dualFoldEndo
            <$> queryFlag
            <*> optional outputOption
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

        , updateCompletionOptions words $ foldEndo
            <$> optional indexOption
            <*> optional Options.shellOption
            <*> optional (setOutput <$> Options.outputOption)
            <*> optional subcommandOption
        ]
  where
    switchTo = Endo . const
    switchToScriptMode = switchTo (ScriptMode defScriptOptions ())
    switchToLibraryMode = switchTo (LibraryMode defLibraryOptions ())
    switchToQueryMode = switchTo (QueryMode defQueryOptions ())
    switchToHelpMode = switchTo (HelpMode ())

    updateCompletionOptions words =
        fmap . mapEndo $ \f _ ->
            let defOpts = CompletionOptions
                    { words
                    , index = Nothing
                    , shell = Options.Bash
                    , subcommand = Nothing
                    , output = OutputStdoutOnly
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

    libraryFlag :: Options.Parser (Endo (CompletionMode ()))
    libraryFlag =
        Options.flag mempty switchToLibraryMode (Options.long "library")

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

    subcommandOption :: HasSubcommand a => Options.Parser (Endo a)
    subcommandOption =
        Options.strOption (Options.long "subcommand") <&> \subcommand ->
            updateSubcommand (Endo \_ -> Just subcommand)

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
                    ScriptMode opts@ScriptOptions{aliases} cfg ->
                        ScriptMode
                            (opts :: ScriptOptions)
                                { aliases = aliases <> [alias]
                                }
                            cfg
                    mode ->
                        mode

    outputOption :: Options.Parser (Endo (CompletionMode ()))
    outputOption = updateOutput <$> Options.outputOption

    helpFlag = Options.flag mempty switchToHelpMode $ mconcat
        [ Options.short 'h'
        , Options.long "help"
        ]

    execParser options parser =
        Options.internalSubcommandParse appNames config "completion"
            Options.defaultPrefs (Options.info parser mempty) options

completionSubcommandHelp :: AppNames -> Pretty.Doc (Result Pretty.AnsiStyle)
completionSubcommandHelp AppNames{usedName} = Pretty.vsep
    [ Pretty.reflow "Command line completion, editor, and IDE support."
    , ""

    , usageSection usedName
        [ "completion"
            <+> Pretty.brackets (longOptionWithArgument "index" "NUM")
            <+> Pretty.brackets (longOptionWithArgument "shell" "SHELL")
            <+> Pretty.brackets (longOptionWithArgument "output" "FILE")
            <+> Pretty.brackets
                (longOptionWithArgument "subcommand" "SUBCOMMAND")
            <+> value "--"
            <+> Pretty.brackets (metavar "WORD" <+> "...")

        , "completion"
            <+> longOption "script"
            <+> Pretty.brackets (longOptionWithArgument "shell" "SHELL")
            <+> Pretty.brackets (longOptionWithArgument "output" "FILE")
            <+> Pretty.brackets
                ( longOptionWithArgument "alias" "ALIAS"
                <+> "..."
                )

        , "completion"
            <+> longOption "script"
            <+> Pretty.brackets (longOptionWithArgument "shell" "SHELL")
            <+> Pretty.brackets (longOptionWithArgument "output" "FILE")
            <+> longOptionWithArgument "subcommand" "SUBCOMMAND"
            <+> longOptionWithArgument "alias" "ALIAS"
            <+> Pretty.brackets
                ( longOptionWithArgument "alias" "ALIAS"
                <+> "..."
                )

        , "completion"
            <+> longOption "query"
            <+> Pretty.brackets (longOptionWithArgument "output" "FILE")
            <+> Pretty.brackets
                ( longOption "subcommands"
                <> "|" <> longOption "subcommand-aliases"
                <> "|" <> longOption "supported-shells"
                <> "|" <> longOption "verbosity-values"
                <> "|" <> longOption "colo[u]r-values"
                )
--          <+> Pretty.brackets (metavar "PATTERN")

        , "completion"
            <+> longOption "library"
            <+> Pretty.brackets (longOptionWithArgument "shell" "SHELL")
            <+> Pretty.brackets (longOptionWithArgument "output" "FILE")

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
            , Pretty.reflow "Supported", metavar "SHELL", "values are: "
            , value "bash", "and", value "fish" <> "."
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
            , Pretty.reflow "Supported", metavar "SHELL", "values are: "
            , value "bash", "and", value "fish" <> "."
            ]

        , optionDescription ["--subcommand=SUBCOMMAND"]
            [ Pretty.reflow "Generate completion script for a"
            , metavar "SUBCOMMAND"
            , Pretty.reflow
                "instead of the whole toolset. At least one instance of"
            , value "--alias=ALIAS"
            , Pretty.reflow "has to be specified."
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
            , longOptionWithArgument "verbosity" "VERBOSITY"
            , Pretty.reflow "option, or are passed down to subcommands via"
            , metavar "COMMAND_WRAPPER_VERBOSITY"
            , Pretty.reflow "environment variable."
            ]

        , optionDescription ["--colo[u]r-values"]
            [ Pretty.reflow "Query possible"
            , metavar "WHEN"
            , Pretty.reflow "colour output values. These can be set using"
            , "global", longOptionWithArgument "colo[u]r" "WHEN"
            , Pretty.reflow "option, or are passed down to subcommands via"
            , metavar "COMMAND_WRAPPER_COLOUR"
            , Pretty.reflow "environment variable."
            ]

--      , optionDescription ["PATTERN"]
--          [ Pretty.reflow "TODO: Document"
--          ]
        ]

    , section "Library options:"
        [ optionDescription ["--library"]
            [ Pretty.reflow "Print a library to standard output that can be\
                \ used by a subcommand."
            ]

        , optionDescription ["--shell=SHELL"]
            [ Pretty.reflow "Print library for", metavar "SHELL" <> "."
            , Pretty.reflow "Currently only supported value is"
            , value "bash" <> "."
            ]
        ]

    , section "Common options:"
        [ optionDescription ["--output=FILE", "-o FILE"]
            [ Pretty.reflow "Write output into", metavar "FILE"
            , Pretty.reflow "instead of standard output."
            ]
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
