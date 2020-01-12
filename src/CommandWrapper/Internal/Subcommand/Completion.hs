{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:      CommandWrapper.Internal.Subcommand.Completion
-- Description: Implementation of internal subcommand that provides command
--              line completion and support for IDE-like functionality.
-- Copyright:   (c) 2018-2020 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions; POSIX.
--
-- Implementation of internal subcommand that provides command line completion
-- and support for IDE-like functionality.
module CommandWrapper.Internal.Subcommand.Completion
    ( completion
    , completionSubcommandCompleter
    , completionSubcommandHelp

    , CompletionConfig(..)
    , InternalCompleter
    , Completer
    )
  where

import Prelude ((+), (-), fromIntegral, maxBound, minBound)

import Control.Applicative ((<*), (<*>), (<|>), many, optional, pure, some)
import Control.Monad ((=<<), (>>=), join, when)
import Data.Bool (Bool(False, True), not, otherwise)
import qualified Data.Char as Char (isDigit, toLower)
import Data.Either (Either(Left, Right))
import Data.Eq (Eq, (/=), (==))
import Data.Foldable (all, any, asum, length, null)
import Data.Function (($), (.), id, on)
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
import System.IO (IO, stderr, stdout)
import Text.Read (readMaybe)
import Text.Show (Show)

import Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI (foldedCase, mk)
import Data.Generics.Product.Typed (typed)
import Data.Monoid.Endo (mapEndo)
import Data.Monoid.Endo.Fold (dualFoldEndo, foldEndo)
import Data.Output
    ( HasOutput(Output)
    , OutputStdoutOrFile
    , pattern OutputStdoutOnly
    , setOutput
    )
import qualified Data.Output as Output (HasOutput(output))
import Data.Text (Text)
import qualified Data.Text as Text (unpack)
import Data.Text.Prettyprint.Doc ((<+>), pretty)
import qualified Data.Text.Prettyprint.Doc as Pretty
    ( Doc
    , braces
    , brackets
    , encloseSep
    , softline
    , squotes
    , vsep
    )
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Pretty (AnsiStyle)
import qualified Data.Text.Prettyprint.Doc.Util as Pretty (reflow)
import qualified Dhall
import qualified Mainplate (applySimpleDefaults)
import qualified Options.Applicative as Options
    ( Parser
    , defaultPrefs
    , eitherReader
    , flag
    , flag'
    , info
    , long
    , metavar
    , option
    , short
    , strOption
    )
import qualified Options.Applicative.Standard as Options (outputOption)
import Safe (atMay, headMay, lastMay)
import System.Process (CreateProcess(env), proc, readCreateProcessWithExitCode)
import Text.Fuzzy as Fuzzy (Fuzzy)
import qualified Text.Fuzzy as Fuzzy (Fuzzy(original), filter)

import qualified CommandWrapper.Config.Global as Global (Config(..), getAliases)
import CommandWrapper.Core.Completion.FileSystem
    ( EntryType(Directory)
    , FileSystemOptions
        ( allowTilde
        , appendSlashToSingleDirectoryResult
        , entryType
        , expandTilde
        , output
        , prefix
        , suffix
        , word
        )
    , defFileSystemOptions
    , fileSystemCompleter
    , outputLines
    , parseEntryType
    , queryFileSystem
    , showEntryType
    )
import CommandWrapper.Core.Environment (AppNames(AppNames, usedName))
import CommandWrapper.Core.Help.Pretty
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
import CommandWrapper.Core.Message (Result, errorMsg, out)
import qualified CommandWrapper.External as External
    ( executeCommand
    , executeCommandWith
    , findSubcommands
    )
import qualified CommandWrapper.Internal.Subcommand.Config.Dhall as Dhall
    ( Exec(arguments)
    , Input(..)
    , defExec
    , exec
    )
import CommandWrapper.Internal.Utils (runMain)
import CommandWrapper.Options.Alias (Alias(alias))
import qualified CommandWrapper.Options.Alias as Options (applyAliasCompletion)
import qualified CommandWrapper.Options.Optparse as Options
    ( internalSubcommandParse
    , splitArguments
    , splitArguments'
    )
import qualified CommandWrapper.Options.Shell as Options
import CommandWrapper.Internal.Subcommand.Completion.Libraries
    ( ImportOrContent(Content, Import)
    , Library(..)
    , LibraryOptions(..)
    , ScriptOptions(..)
    , defLibraryOptions
    , defScriptOptions
    , parseDhallLibrary
    , putLibrary
    , putShellCompletionScript
    , showDhallLibrary
    )


-- | Returns completer if the first argument is a name of an internal command.
type InternalCompleter = String -> Maybe Completer

type Completer =
        AppNames
    ->  Global.Config
    ->  Options.Shell
    ->  Word
    ->  [String]
    ->  IO [String]

data MatchingAlgorithm
    = PrefixEquivalence
    | FuzzyMatch
    | Equality
  deriving stock (Generic, Show)

data Query = Query
    { what :: WhatToQuery

    , patternToMatch :: String
    -- ^ TODO: This conflates pattern not present and empty pattern.  Maybe we
    -- should support multiple patterns, in which case this will nicely unite
    -- with 'CompletionOptions'.

    , matchingAlgorithm :: MatchingAlgorithm

    , caseSensitive :: Bool
    -- ^ Be case sensitive when pattern matching values.
    --
    -- * @False@: don't be case sensitive when pattern matching.
    -- * @True@: be case sensitive when pattern matching.

    , output :: OutputStdoutOrFile

    , prefix :: String
    , suffix :: String

    , enableTildeExpansion :: Bool
    , substituteTilde :: Bool
    }
  deriving stock (Generic, Show)
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
    | QueryInternalSubcommands
    | QuerySubcommandAliases
--  | QueryExternalSubcommand
    | QueryVerbosityValues
    | QueryColourValues
    | QuerySupportedShells
    | QueryWords [String]
    | QueryFileSystem EntryType
  deriving stock (Generic, Show)

defQueryOptions :: Query
defQueryOptions = Query
    { what = QuerySubcommands
    , patternToMatch = ""
    , matchingAlgorithm = PrefixEquivalence
    , caseSensitive = False
    , output = OutputStdoutOnly
    , prefix = ""
    , suffix = ""

    -- Thses values mimic default `compgen` behaviour:
    , enableTildeExpansion = True
    , substituteTilde = True
    }

data CompletionMode a
    = CompletionMode CompletionOptions a
    | ScriptMode ScriptOptions a
    | LibraryMode LibraryOptions a
    | QueryMode Query a
    | WrapperMode WrapperOptions a
    | HelpMode a
  deriving stock (Functor, Generic, Show)

updateOutput :: OutputStdoutOrFile -> Endo (CompletionMode b)
updateOutput o = Endo \case
    CompletionMode opts a -> CompletionMode (setOutput o opts) a
    ScriptMode opts a -> ScriptMode (setOutput o opts) a
    LibraryMode opts a -> LibraryMode (setOutput o opts) a
    QueryMode opts a -> QueryMode (setOutput o opts) a
    WrapperMode opts a -> WrapperMode opts a
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

type CompletionInfo = Options.Shell -> Natural -> [Text] -> [Text]

instance Options.HasShell CompletionOptions where
    updateShell =
        mapEndo \f opts@CompletionOptions{shell} ->
            (opts :: CompletionOptions){shell = f shell}

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

class HasPattern a where
    updatePattern :: Endo String -> Endo a

instance HasPattern Query where
    updatePattern :: Endo String -> Endo Query
    updatePattern (Endo f) = Endo \q@Query{patternToMatch} ->
        (q  :: Query){patternToMatch = f patternToMatch}

setPattern :: HasPattern a => String -> Endo a
setPattern pat = updatePattern (Endo \_ -> pat)

data WrapperOptions = WrapperOptions
    { arguments :: [String]
    , expression :: Text
    -- ^ Dhall expression to execute.
    }
  deriving stock (Generic, Show)

-- | Configuration of @completion@ internal subcommand.
data CompletionConfig = CompletionConfig
    { internalCompleter :: InternalCompleter
    -- ^ Completer for all internal subcommands.

    , internalSubcommands :: [String]
    -- ^ Names of internal subcommands.
    }

completion
    :: CompletionConfig
    -> AppNames
    -> [String]
    -> Global.Config
    -> IO ()
completion completionConfig@CompletionConfig{..} appNames options config =
    runMain (parseOptions appNames config options) defaults \case
        CompletionMode opts@CompletionOptions{output} config' ->
            getCompletions completionConfig appNames config' opts
                >>= outputLines id output

        -- TODO:
        --
        -- - Completion script should be configurable.
        ScriptMode opts config' ->
            putShellCompletionScript subcommand' appNames config' opts

        LibraryMode opts config' ->
            putLibrary subcommand' config' opts

        QueryMode query@Query{..} config' -> case what of
            QuerySubcommands
              | null patternToMatch ->
                    getSubcommands appNames config' internalSubcommands
                        >>= outputQueryLines query output

              | otherwise ->
                    outputQueryLines query output =<< case matchingAlgorithm of
                        PrefixEquivalence ->
                            findSubcommandsPrefix config' query

                        FuzzyMatch ->
                            fmap Fuzzy.original
                                <$> findSubcommandsFuzzy config' query

                        Equality ->
                            List.filter (== patternToMatch)
                                <$> getSubcommands appNames config'
                                        internalSubcommands

            QueryInternalSubcommands
              | null patternToMatch ->
                    outputQueryLines query output internalSubcommands

              | otherwise ->
                    outputQueryLines query output case matchingAlgorithm of
                        PrefixEquivalence ->
                            prefixEquivalence query internalSubcommands

                        FuzzyMatch ->
                            Fuzzy.original
                                <$> fuzzyMatch query internalSubcommands

                        Equality ->
                            List.filter (== patternToMatch) internalSubcommands

            QuerySubcommandAliases
              | null patternToMatch ->
                    outputQueryLines query output (getAliases config')

              | otherwise ->
                    outputQueryLines query output case matchingAlgorithm of
                        PrefixEquivalence ->
                            findSubcommandAliasesPrefix config' query

                        FuzzyMatch ->
                            Fuzzy.original
                                <$> findSubcommandAliasesFuzzy config' query

                        Equality ->
                            List.filter (== patternToMatch) (getAliases config')

            QueryVerbosityValues
              | null patternToMatch ->
                    outputQueryLines query output verbosityValues

              | otherwise ->
                    outputQueryLines query output case matchingAlgorithm of
                        PrefixEquivalence ->
                            prefixEquivalence query verbosityValues

                        FuzzyMatch ->
                            Fuzzy.original <$> fuzzyMatch query verbosityValues

                        Equality ->
                            List.filter (== patternToMatch) verbosityValues

            QueryColourValues
              | null patternToMatch ->
                    outputQueryLines query output colourValues

              | otherwise ->
                    outputQueryLines query output case matchingAlgorithm of
                        PrefixEquivalence ->
                            prefixEquivalence query colourValues

                        FuzzyMatch ->
                            Fuzzy.original <$> fuzzyMatch query colourValues

                        Equality ->
                            List.filter (== patternToMatch) colourValues

            QuerySupportedShells
              | null patternToMatch ->
                    outputQueryLines query output supportedShells

              | otherwise ->
                    outputQueryLines query output case matchingAlgorithm of
                        PrefixEquivalence ->
                            prefixEquivalence query supportedShells

                        FuzzyMatch ->
                            Fuzzy.original <$> fuzzyMatch query supportedShells

                        Equality ->
                            List.filter (== patternToMatch) supportedShells

            QueryFileSystem entryType ->
                queryFileSystem defFileSystemOptions
                    { entryType = Just entryType
                    , output
                    , prefix
                    , suffix
                    , word = patternToMatch
                    , allowTilde = enableTildeExpansion
                    , expandTilde = substituteTilde
                    }

            QueryWords words
              | null patternToMatch ->
                    outputQueryLines query output words

              | otherwise ->
                    outputQueryLines query output case matchingAlgorithm of
                        PrefixEquivalence ->
                            prefixEquivalence query words

                        FuzzyMatch ->
                            Fuzzy.original <$> fuzzyMatch query words

                        Equality ->
                            List.filter (== patternToMatch) words

        WrapperMode WrapperOptions{arguments, expression} config' -> do
            Dhall.exec appNames config'
                ( (Dhall.defExec $ Dhall.InputExpression expression)
                    { Dhall.arguments = fromString <$> arguments
                    }
                )

        HelpMode config'@Global.Config{colourOutput, verbosity} ->
            out verbosity colourOutput stdout
                (completionSubcommandHelp appNames config')
  where
    defaults =
        let opts = CompletionOptions
                { words = []
                , index = Nothing
                , shell = Options.Bash
                , subcommand = Nothing
                , output = OutputStdoutOnly
                }

        in Mainplate.applySimpleDefaults (CompletionMode opts config)

    -- TODO: Figure out how to handle this better.
    subcommand' :: forall ann. Pretty.Doc ann
    subcommand' = pretty (usedName appNames <> " completion")

    getAliases :: Global.Config -> [String]
    getAliases = List.nub . fmap alias . Global.getAliases

    findSubcommandsFuzzy :: Global.Config -> Query -> IO [Fuzzy String String]
    findSubcommandsFuzzy cfg query = do
        cmds <- getSubcommands appNames cfg internalSubcommands
        pure (fuzzyMatch query cmds)

    fuzzyMatch :: Query -> [String] -> [Fuzzy String String]
    fuzzyMatch Query{patternToMatch, caseSensitive} possibilities =
        Fuzzy.filter patternToMatch possibilities "" "" id caseSensitive

    findSubcommandsPrefix :: Global.Config -> Query -> IO [String]
    findSubcommandsPrefix cfg query =
        prefixEquivalence query
            <$> getSubcommands appNames cfg internalSubcommands

    prefixEquivalence :: Query -> [String] -> [String]
    prefixEquivalence Query{patternToMatch, caseSensitive} possibilities =
        List.filter match possibilities
      where
        match
          | caseSensitive = (patternToMatch `List.isPrefixOf`)
          | otherwise     = (CI.mk patternToMatch `ciIsPrefixOf`) . CI.mk

    findSubcommandAliasesFuzzy
        :: Global.Config
        -> Query
        -> [Fuzzy String String]
    findSubcommandAliasesFuzzy cfg query = fuzzyMatch query (getAliases cfg)

    findSubcommandAliasesPrefix :: Global.Config -> Query -> [String]
    findSubcommandAliasesPrefix cfg query =
        prefixEquivalence query (getAliases cfg)

    outputQueryLines :: Query -> OutputStdoutOrFile -> [String] -> IO ()
    outputQueryLines Query{prefix, suffix} =
        outputLines \s -> prefix <> s <> suffix

-- TODO: Generate these values instead of hard-coding them.
verbosityValues :: [String]
verbosityValues = ["silent", "quiet", "normal", "verbose", "annoying"]

-- TODO: Generate these values instead of hard-coding them.
colourValues :: [String]
colourValues = ["always", "auto", "never"]

-- TODO: Generate these values instead of hard-coding them.
supportedShells :: [String]
supportedShells = ["bash", "fish", "zsh"]

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
            , ("--aliases", Nothing)
            , ("--no-aliases", Nothing)
            , ("--no-color", Nothing)
            , ("--no-colour", Nothing)
            , ("--version", Nothing)
            , ("--verbosity=", Just (matchKeywords verbosityValues))
            , ("--change-directory=", Nothing)
            , ("--silent", Nothing)
            , ("--quiet", Nothing)
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
getCompletions
    :: CompletionConfig
    -> AppNames
    -> Global.Config
    -> CompletionOptions
    -> IO [String]
getCompletions CompletionConfig{..} appNames config CompletionOptions{..} =
    case subcommand of
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

    subcommandCompletion' idx subcommandArguments subcommandName =
        let -- TODO: Figure out how to take arguments into account when
            -- applying aliases.
            (realSubcommandName, realSubcommandArguments, realIndex) =
                Options.applyAliasCompletion (Global.getAliases config)
                    subcommandName subcommandArguments idx

         in case internalCompleter realSubcommandName of
                Nothing ->
                    subcommandCompletion appNames config shell realIndex
                        realSubcommandArguments subcommandName
                        realSubcommandName

                Just completer ->
                    completer appNames config shell realIndex
                        realSubcommandArguments

    globalCompletion :: IO [String]
    globalCompletion =
        let pat = fromMaybe "" $ atMay words (fromIntegral index')
            subcommands = findSubcommands appNames config internalSubcommands pat

         in case headMay pat of
                Nothing ->
                    (matchGlobalOptions ('-' : pat) <>) <$> subcommands

                Just '-'
                  | "--change-directory=" `List.isPrefixOf` pat ->
                        let prefix = "--change-directory="
                        in  fileSystemCompleter defFileSystemOptions
                                { appendSlashToSingleDirectoryResult = True
                                , entryType = Just Directory
                                , expandTilde = True
                                , prefix
                                , word = List.drop (length prefix) pat
                                }

                  | otherwise ->
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
subcommandCompletion appNames config shell index words _invokedAs subcommand =
  do
    arguments <- completionInfo
        <*> pure shell
        <*> pure (fromIntegral index)
        <*> pure (fromString <$> words)

    External.executeCommand appNames config subcommand
        (Text.unpack <$> arguments)
  where
    -- Implementation of `command-wrapper-subcommand-protocol(7)` for command
    -- line completion.
    completionInfo :: IO CompletionInfo
    completionInfo = do
        (exitCode, outH, errH)
            <- External.executeCommandWith readProcess appNames config
                subcommand ["--completion-info"]

        when (exitCode /= ExitSuccess) $ do
            let Global.Config{colourOutput, verbosity} = config

                subcommand' :: forall ann. Pretty.Doc ann
                subcommand' = pretty
                    $ usedName appNames <> " completion"

            errorMsg subcommand' verbosity colourOutput stderr
                $ fromString subcommand
                <> ": Subcommand protocol violated when called with\
                    \ '--completion-info':\n"
                <> fromString errH
            exitWith (ExitFailure 2)

        Dhall.input Dhall.auto (fromString outH)
      where
        readProcess cmd _ args env =
            readCreateProcessWithExitCode (proc cmd args){env} ""

-- | Command line completion for the @completion@ subcommand itself.
completionSubcommandCompleter
    :: [String]
    -- ^ Names of internal subcommands so that we can complete them.
    -> Completer
completionSubcommandCompleter internalSubcommands appNames config _shell index
  words
  | Just "-o" <- lastBeforePattern =
        fsCompleter ""

  | Just "--output" <- lastBeforePattern =
        fsCompleter ""

  | "--output=" `List.isPrefixOf` pat =
        fsCompleter "--output="

  | "--shell=" `List.isPrefixOf` pat =
        pure if had "--library"
            then List.filter (pat `List.isPrefixOf`) ["--shell=bash"]
            else List.filter (pat `List.isPrefixOf`) shellOptions

  | "--subcommand=" `List.isPrefixOf` pat =
        fmap ("--subcommand=" <>)
            <$> findSubcommands appNames config internalSubcommands
                    (List.drop (length @[] "--subcommand=") pat)

  | had "--library", "--dhall=" `List.isPrefixOf` pat =
      pure (List.filter (pat `List.isPrefixOf`) dhallLibraryOptions)

  | hadOneOf ["--help", "-h"] =
        pure []

  | any ("--index=" `List.isPrefixOf`) wordsBeforePattern =
        pure $ List.filter (pat `List.isPrefixOf`) completionOptions

  | had "--query" =
        pure $ List.filter (pat `List.isPrefixOf`) if
          | hadFileSystemOption ->
                (subsequentQueryOptions <> queryFileSystemSpecificOptions)
          | hadOneOf whatToQueryOptions ->
                subsequentQueryOptions
          | otherwise ->
                whatToQueryOptions <> queryFileSystemOptions

  | had "--library" =
        pure $ List.filter (pat `List.isPrefixOf`) libraryOptions

  | had "--script" =
        pure $ List.filter (pat `List.isPrefixOf`) scriptOptions

  | had "--wrapper" =
        pure $ List.filter (pat `List.isPrefixOf`) wrapperOptions

  | null pat  = pure modeOptions
  | otherwise = pure $ List.filter (pat `List.isPrefixOf`) modeOptions
  where
    wordsBeforePattern = List.take (fromIntegral index) words
    pat = fromMaybe "" $ atMay words (fromIntegral index)
    lastBeforePattern = lastMay wordsBeforePattern

    hadOneOf = any (`List.elem` wordsBeforePattern)
    had = (`List.elem` wordsBeforePattern)

    outputOptions = ["--output=", "-o"]

    whatToQueryOptions =
        ["--subcommands", "--subcommand-aliases", "--supported-shells"
        , "--verbosity-values", "--colour-values", "--color-values"
        , "--internal-subcommands", "--words"
        ]

    queryFileSystemOptions =
        [minBound..maxBound] <&> \s ->
            "--file-system=" <> showEntryType s

    hadFileSystemOption =
        any ("--file-system=" `List.isPrefixOf`) wordsBeforePattern

    subsequentQueryOptions =
        ["--pattern=", "--prefix=", "--suffix="]
        <> outputOptions
        <> algorithmOptions

    queryFileSystemSpecificOptions =
        [ "--substitute-tilde", "--no-substitute-tilde"
        , "--tilde-expansion", "--no-tilde-expansion"
        ]

    -- At the moment we have library only for Bash.
    libraryOptions = ["--shell=bash" , "--import" , "--content", "--dhall="]

    dhallLibraryOptions = [minBound..maxBound] <&> \lib ->
        "--dhall=" <> showDhallLibrary lib

    completionOptions = ["--shell=", "--subcommand=", "--"] <> outputOptions

    scriptOptions = ["--alias=", "--shell=", "--subcommand="] <> outputOptions

    shellOptions = ("--shell=" <>) <$> supportedShells

    wrapperOptions = [ "--expression=", "--exec", "--" ]

    modeOptions = List.nub
        [ "--index="
        , "--library"
        , "--query"
        , "--script"
        , "--wrapper"
        , "--help", "-h"
        ]

    algorithmOptions = ("--algorithm=" <>) <$> ["fuzzy", "prefix", "equality"]

    fsCompleter prefix =
        fileSystemCompleter defFileSystemOptions
            { appendSlashToSingleDirectoryResult = True
            , expandTilde = not (null prefix)
            , prefix
            , word = List.drop (length prefix) pat
            }

-- | Lookup external and internal subcommands matching pattern (prefix).
findSubcommands
    :: AppNames
    -> Global.Config
    -> [String]
    -- ^ List of internal subcommand names.
    -> String
    -- ^ Pattern (prefix) to match subcommand name against.
    -> IO [String]
findSubcommands appNames config internalCommands pat =
    List.filter (fmap Char.toLower pat `List.isPrefixOf`)
        <$> getSubcommands appNames config internalCommands

-- | List all available external and internal subcommands.
getSubcommands :: AppNames -> Global.Config -> [String] -> IO [String]
getSubcommands appNames config internalCommands = do
    extCmds <- External.findSubcommands appNames config

    let aliases = alias <$> Global.getAliases config

    pure (List.nub $ aliases <> internalCommands <> extCmds)

parseOptions
    :: AppNames
    -> Global.Config
    -> [String]
    -> IO (Endo (CompletionMode Global.Config))
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
                    :: Options.Parser (Endo (CompletionMode Global.Config))
                , dualFoldEndo
                    <$> subcommandOption
                    <*> some aliasOption
                , pure mempty
                ]

        , dualFoldEndo
            <$> ( libraryFlag
                <*> ( dualFoldEndo
                    <$> many (importFlag <|> contentFlag)
                    <*> many (Options.shellOption <|> dhallLibraryOption)
                    )
                )
            <*> optional outputOption

        , dualFoldEndo
            <$> queryFlag
            <*> optional outputOption
            <*> ( updateQueryOptions
                    (   subcommandsFlag
                    <|> internalSubcommandsFlag
                    <|> subcommandAliasesFlag
                    <|> supportedShellsFlag
                    <|> verbosityValuesFlag
                    <|> colourValuesFlag
                    <|> wordsFlag words
                    <|> ( dualFoldEndo
                            <$> fileSystemOption
                            <*> many tildeExpansionFlag
                            <*> many noTildeExpansionFlag
                            <*> many substituteTildeFlag
                            <*> many noSubstituteTildeFlag
                        )
                    )
                )
            <*> optional (updateQueryOptions patternOption)
            <*> optional (updateQueryOptions algorithmOption)
            <*> optional
                ( updateQueryOptions $ prefixOption \prefix opts ->
                    (opts :: Query){prefix}
                )
            <*> optional
                ( updateQueryOptions $ suffixOption \suffix opts ->
                    (opts :: Query){suffix}
                )
--          <*> updateQueryOptions
--              ( pure $ Endo \q ->
--                  -- TODO: This is not very intuitive if user passes more
--                  -- than one pattern on the command line.  What we should do
--                  -- is fail with a reasonable error message.
--                  q{patternToMatch = fromMaybe "" (headMay words)}
--              )

        , dualFoldEndo
            <$> ( wrapperFlag
                <*> pure words
                <*> expressionOption
                <*  execFlag
                )

        , helpFlag

        , updateCompletionOptions words $ foldEndo
            <$> optional indexOption
            <*> optional Options.shellOption
            <*> optional (setOutput <$> Options.outputOption)
            <*> optional subcommandOption
        ]
  where
    switchTo :: (a -> CompletionMode a) -> Endo (CompletionMode a)
    switchTo f = Endo \case
        CompletionMode _ a -> f a
        ScriptMode _ a -> f a
        LibraryMode _ a -> f a
        QueryMode _ a -> f a
        WrapperMode _ a -> f a
        HelpMode a -> f a

    switchToScriptMode :: Endo (CompletionMode Global.Config)
    switchToScriptMode = switchTo (ScriptMode defScriptOptions)

    switchToQueryMode :: Endo (CompletionMode Global.Config)
    switchToQueryMode = switchTo (QueryMode defQueryOptions)

    switchToHelpMode :: Endo (CompletionMode Global.Config)
    switchToHelpMode = switchTo HelpMode

    switchToLibraryMode
        :: Endo LibraryOptions
        -> Endo (CompletionMode Global.Config)
    switchToLibraryMode (Endo f) =
        switchTo (LibraryMode $ f defLibraryOptions)

    switchToWrapperMode
        :: [String]
        -> Text
        -> Endo (CompletionMode Global.Config)
    switchToWrapperMode args expression =
        switchTo (WrapperMode WrapperOptions{arguments = args, expression})

    updateCompletionOptions
        :: [String]
        -> Options.Parser (Endo CompletionOptions)
        -> Options.Parser (Endo (CompletionMode a))
    updateCompletionOptions words =
        fmap . mapEndo $ \f ->
            let defOpts = CompletionOptions
                    { words
                    , index = Nothing
                    , shell = Options.Bash
                    , subcommand = Nothing
                    , output = OutputStdoutOnly
                    }
            in appEndo $ switchTo (CompletionMode (f defOpts))

    updateQueryOptions
        :: Options.Parser (Endo Query)
        -> Options.Parser (Endo (CompletionMode Global.Config))
    updateQueryOptions = fmap . mapEndo $ \f -> \case
        QueryMode q cfg -> QueryMode (f q) cfg
        mode            -> mode

    scriptFlag :: Options.Parser (Endo (CompletionMode Global.Config))
    scriptFlag = Options.flag mempty switchToScriptMode (Options.long "script")

    libraryFlag
        :: Options.Parser
            ( Endo LibraryOptions
            -> Endo (CompletionMode Global.Config)
            )
    libraryFlag =
        Options.flag mempty switchToLibraryMode (Options.long "library")

    importFlag :: Options.Parser (Endo LibraryOptions)
    importFlag =
        Options.flag' setValue (Options.long "import")
      where
        setValue = Endo \opts -> opts{importOrContent = Import}

    contentFlag :: Options.Parser (Endo LibraryOptions)
    contentFlag =
        Options.flag' setValue (Options.long "content")
      where
        setValue = Endo \opts -> opts{importOrContent = Content}

    dhallLibraryOption :: Options.Parser (Endo LibraryOptions)
    dhallLibraryOption =
        Options.option parse (Options.long "dhall" <> Options.metavar "LIBRARY")
      where
        parse = Options.eitherReader \s ->
            case parseDhallLibrary (CI.mk s) of
                Just lib ->
                    Right $ Endo \opts ->
                        opts{library = DhallLibrary lib}
                Nothing ->
                    Left "Unknown Dhall library"

    queryFlag :: Options.Parser (Endo (CompletionMode Global.Config))
    queryFlag = Options.flag mempty switchToQueryMode (Options.long "query")

    subcommandsFlag :: Options.Parser (Endo Query)
    subcommandsFlag = Options.flag mempty f (Options.long "subcommands")
      where
        f = Endo \q -> q{what = QuerySubcommands}

    internalSubcommandsFlag :: Options.Parser (Endo Query)
    internalSubcommandsFlag =
        Options.flag mempty f (Options.long "internal-subcommands")
      where
        f = Endo \q -> q{what = QueryInternalSubcommands}

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

    fileSystemOption :: Options.Parser (Endo Query)
    fileSystemOption = Options.option parse
        (Options.long "file-system" <> Options.metavar "TYPE")
      where
        parse = Options.eitherReader \s ->
            case parseEntryType (CI.mk s) of
                Just entryType ->
                    Right $ Endo \opts ->
                        opts{what = QueryFileSystem entryType}
                Nothing ->
                    Left "Unknown file system entry type"

    tildeExpansionFlag :: Options.Parser (Endo Query)
    tildeExpansionFlag =
        Options.flag' f (Options.long "tilde-expansion")
      where
        f = Endo \q -> q{enableTildeExpansion = True}

    noTildeExpansionFlag :: Options.Parser (Endo Query)
    noTildeExpansionFlag =
        Options.flag' f (Options.long "no-tilde-expansion")
      where
        f = Endo \q -> q{enableTildeExpansion = False}

    substituteTildeFlag :: Options.Parser (Endo Query)
    substituteTildeFlag = Options.flag' f (Options.long "substitute-tilde")
      where
        f = Endo \q -> q{substituteTilde = True}

    noSubstituteTildeFlag :: Options.Parser (Endo Query)
    noSubstituteTildeFlag =
        Options.flag' f (Options.long "no-substitute-tilde")
      where
        f = Endo \q -> q{substituteTilde = False}

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

    aliasOption :: Options.Parser (Endo (CompletionMode Global.Config))
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

    outputOption :: Options.Parser (Endo (CompletionMode Global.Config))
    outputOption = updateOutput <$> Options.outputOption

    wrapperFlag
        :: Options.Parser
            ([String] -> Text -> Endo (CompletionMode Global.Config))
    wrapperFlag =
        Options.flag mempty switchToWrapperMode (Options.long "wrapper")

    expressionOption :: Options.Parser Text
    expressionOption = Options.strOption
        (Options.long "expression" <> Options.metavar "EXPRESSION")

    execFlag :: Options.Parser ()
    execFlag = Options.flag' () (Options.long "exec")
        -- TODO: At the moment '--exec' does nothing, but it will become
        -- relevant when we introduce '--interpreter=COMMAND'.

    patternOption :: HasPattern a => Options.Parser (Endo a)
    patternOption = setPattern <$> Options.strOption (Options.long "pattern")

    algorithmOption :: Options.Parser (Endo Query)
    algorithmOption =
        Options.option parse
            (Options.long "algorithm" <> Options.metavar "ALGORITHM")
      where
        parse = Options.eitherReader \s -> case CI.mk s of
            "fuzzy" ->
                Right $ Endo \q ->
                    q{matchingAlgorithm = FuzzyMatch}

            "prefix" ->
                Right $ Endo \q ->
                    q{matchingAlgorithm = PrefixEquivalence}

            "equality" ->
                Right $ Endo \q ->
                    q{matchingAlgorithm = Equality}

            _ ->
                Left "Unknown matching algorithm"

    wordsFlag :: [String] -> Options.Parser (Endo Query)
    wordsFlag ws = Options.flag mempty f (Options.long "words")
      where
        f = Endo \q -> q{what = QueryWords ws}

    prefixOption :: (String -> a -> a) -> Options.Parser (Endo a)
    prefixOption f = Endo . f <$> Options.strOption
        (Options.long "prefix" <> Options.metavar "STRING")

    suffixOption :: (String -> a -> a) -> Options.Parser (Endo a)
    suffixOption f = Endo . f <$> Options.strOption
        (Options.long "suffix" <> Options.metavar "STRING")

    helpFlag = Options.flag mempty switchToHelpMode $ mconcat
        [ Options.short 'h'
        , Options.long "help"
        ]

    execParser options parser =
        Options.internalSubcommandParse appNames config "completion"
            Options.defaultPrefs (Options.info parser mempty) options

completionSubcommandHelp
    :: AppNames
    -> Global.Config
    -> Pretty.Doc (Result Pretty.AnsiStyle)
completionSubcommandHelp AppNames{usedName} _config = Pretty.vsep
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
            <+> Pretty.braces
                (         longOption "subcommands"
                <> "|" <> longOption "subcommand-aliases"
                <> "|" <> longOption "supported-shells"
                <> "|" <> longOption "verbosity-values"
                <> "|" <> longOption "colo[u]r-values"
                )
            <+> Pretty.brackets (longOptionWithArgument "algorithm" "ALGORITHM")
            <+> Pretty.brackets (longOptionWithArgument "pattern" "PATTERN")
            <+> Pretty.brackets (longOptionWithArgument "prefix" "STRING")
            <+> Pretty.brackets (longOptionWithArgument "suffix" "STRING")
            <+> Pretty.brackets (longOptionWithArgument "output" "FILE")

        , "completion"
            <+> longOption "query"
            <+> longOptionWithArgument "file-system" "TYPE"
            <+> Pretty.brackets (longOptionWithArgument "algorithm" "ALGORITHM")
            <+> Pretty.brackets (longOptionWithArgument "pattern" "PATTERN")
            <+> Pretty.brackets
                ( longOption "[no-]tilde-expansion"
                <+> Pretty.brackets (longOption "[no-]substitute-tilde")
                )
            <+> Pretty.brackets (longOptionWithArgument "prefix" "STRING")
            <+> Pretty.brackets (longOptionWithArgument "suffix" "STRING")
            <+> Pretty.brackets (longOptionWithArgument "output" "FILE")

        , "completion"
            <+> longOption "query"
            <+> longOption "words"
            <+> Pretty.brackets (longOptionWithArgument "algorithm" "ALGORITHM")
            <+> Pretty.brackets (longOptionWithArgument "pattern" "PATTERN")
            <+> Pretty.brackets (longOptionWithArgument "prefix" "STRING")
            <+> Pretty.brackets (longOptionWithArgument "suffix" "STRING")
            <+> Pretty.brackets (longOptionWithArgument "output" "FILE")
            <+> Pretty.brackets "--"
            <+> Pretty.brackets (metavar "WORD" <+> "...")

        , "completion"
            <+> longOption "library"
            <+> Pretty.brackets
                ( longOptionWithArgument "shell" "SHELL"
                <> "|" <> longOptionWithArgument "dhall" "LIBRARY"
                )
            <+> Pretty.brackets
                (         longOption "import"
                <> "|" <> longOption "content"
                )
            <+> Pretty.brackets (longOptionWithArgument "output" "FILE")

        , "completion"
            <+> longOption "wrapper"
            <+> longOptionWithArgument "expression" "EXPRESSION"
            <+> longOption "exec"

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
            , value "bash" <> ",", value "fish" <> ",", "and"
            , value "zsh" <> "."
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
            , value "bash" <> ",", value "fish" <> ",", "and"
            , value "zsh" <> "."
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

    , section "Common query options:"
        [ optionDescription ["--query"]
            [ Pretty.reflow "Query command line interface."
            , Pretty.reflow "Useful for editor/IDE integration."
            ]

        , optionDescription ["--algorithm=ALGORITHM"]
            [ Pretty.reflow "Specify which pattern matching algorithm to use"
            , "when", longOptionWithArgument "pattern" "PATTERN"
            , Pretty.reflow "is provided. Possible values are:"
            , value "prefix" <> ",", value "fuzzy" <> ",", "and"
            , value "equality."
            ]

        , optionDescription ["--pattern=PATTERN"]
            [ Pretty.reflow "Print only values that are matching"
            , metavar "PATTERN" <> "."
            ]

        , optionDescription ["--prefix=STRING"]
            [ "Prepend", metavar "STRING"
            , Pretty.reflow "to every result entry."
            ]

        , optionDescription ["--suffix=STRING"]
            [ "Append", metavar "STRING"
            , Pretty.reflow "to every result entry."
            ]
        ]

    , section "Query Command Wrapper options:"
        [ optionDescription ["--subcommands"]
            [ Pretty.reflow "Query all available subcommands. This includes\
                \ internal subcommands, external subcommands, and subcommand\
                \ aliases."
            ]

--      , optionDescription ["--fail-when-no-match"]
--          [ Pretty.reflow "Terminate with exit code 3 if no match was found."
--          ]

--      , optionDescription ["--external-subcommands"]
--          [ Pretty.reflow "Query available external subcommands."
--          ]

        , optionDescription ["--internal-subcommands"]
            [ Pretty.reflow "Query available internal subcommands."
            ]

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
        ]

    , section "Query file system options:"
        [ optionDescription ["--file-system=TYPE"]
            [ Pretty.reflow "Query for entries of"
            , metavar "TYPE" <> ","
            , "where"
            , metavar "TYPE"
            , "is one of:"
            , Pretty.encloseSep mempty mempty ("," <> Pretty.softline)
                (value . showEntryType <$> [minBound..maxBound])
            ]

        , optionDescription ["--[no-]tilde-expansion"]
            [ Pretty.reflow "Interpret prefix" , value "~" , "in"
            , metavar "PATTERN"
            , Pretty.reflow "as current user's home directory."
            ]

        , optionDescription ["--[no-]substitute-tilde"]
            [ Pretty.reflow "Substitute prefix", value "~"
            , Pretty.reflow "in completions for full path to current user's\
                \ home directory."
            ]
        ]

    , section "Query words options:"
        [ optionDescription ["--words"]
            [ Pretty.reflow "Query matching words from"
            , metavar "WORD", "list."
            ]

        , optionDescription ["WORD"]
            [ Pretty.reflow "Potential completions that are matched against"
            , longOptionWithArgument "pattern" "PATTERN" <> "."
            ]
        ]

    , section "Library options:"
        [ optionDescription ["--library"]
            [ Pretty.reflow "Print a library to standard output that can be\
                \ used by a subcommand or a configuration file.  Option"
            , longOptionWithArgument "shell" "SHELL", "and"
            , longOptionWithArgument "dhall" "LIBRARY"
            , Pretty.reflow
                "are control which library is produced.  If neither"
            , longOptionWithArgument "shell" "SHELL", "nor"
            , longOptionWithArgument "dhall" "LIBRARY", "then"
            , longOptionWithArgument "shell" "bash"
            , Pretty.reflow "is assumed."
            ]

        , optionDescription ["--shell=SHELL"]
            [ Pretty.reflow "Print library for", metavar "SHELL" <> ","
            , Pretty.reflow "or import snippet when", longOption "import"
            , Pretty.reflow "is specified. Currently only supported value is"
            , value "bash" <> "."
            ]

        , optionDescription ["--dhall=LIBRARY"]
            [ Pretty.reflow "Print specified Dhall", metavar "LIBRARY" <> ","
            , Pretty.reflow "or its import snippet when", longOption "import"
            , Pretty.reflow "is specified.  Supported values of"
            , metavar "LIBRARY", "are:"
            , Pretty.encloseSep mempty mempty ("," <> Pretty.softline)
                (value . showDhallLibrary <$> [minBound..maxBound])
            ]

        , optionDescription ["--import"]
            [ Pretty.reflow "Print code snipped for importing", metavar "SHELL"
            , Pretty.reflow "or Dhall", metavar "LIBRARY" <> "."
            , Pretty.reflow "If neither"
            , longOption "content" <> ",", "nor", longOption "import"
            , Pretty.reflow "is specified then", longOption "content"
            , Pretty.reflow "is assumed."
            ]

        , optionDescription ["--content"]
            [ Pretty.reflow "Print content of the Command Wrapper library for"
            , metavar "SHELL" <> "."
            , Pretty.reflow "This is the default behavior if neither"
            , longOption "content" <> ",", "nor", longOption "import"
            , Pretty.reflow "is specified."
            ]
        ]

    , section "Wrapper options:"
        [ optionDescription ["--wrapper"]
            [ Pretty.reflow "Generate and execute a script for command line\
                \ completion.  Useful when reusing existing completion\
                \ scripts."
            ]
        , optionDescription ["--expression=EXPRESSION"]
            [ "Dhall", metavar "EXPRESSION", Pretty.reflow "to be used to\
                \ generate executable script."
            ]
        , optionDescription ["--exec"]
            [ Pretty.reflow "Execute generated script directly."
              -- TODO: Mention --interpreter=COMMAND when implemented.
            ]
--      , optionDescription ["--interpreter=COMMAND"]
--          [ Pretty.reflow "TODO"
--          ]
--      , optionDescription ["--interpreter-argument=ARGUMENT"]
--          [ Pretty.reflow "TODO"
--          ]
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

ciIsPrefixOf :: forall a. Eq a => CI [a] -> CI [a] -> Bool
ciIsPrefixOf = List.isPrefixOf `on` CI.foldedCase
