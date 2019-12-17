{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
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
    ( completion
    , completionSubcommandCompleter
    , completionSubcommandHelp

    , CompletionConfig(..)
    , InternalCompleter
    , Completer
    )
  where

import Prelude ((+), (-), fromIntegral)

import Control.Applicative ((<*), (<*>), (<|>), many, optional, pure, some)
import Control.Monad ((=<<), (>>=), join, when)
import Data.Bool (Bool(False), otherwise)
import qualified Data.Char as Char (isDigit, toLower)
import Data.Either (Either(Left, Right))
import Data.Eq (Eq, (/=), (==))
import Data.Foldable (all, any, asum, forM_, length, mapM_, null)
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
import System.IO
    ( Handle
    , IO
    , IOMode(WriteMode)
    , putStrLn
    , stderr
    , stdout
    , withFile
    )
import Text.Read (readMaybe)
import Text.Show (Show, show)

import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString (putStr)
import Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI (foldedCase, mk)
import Data.Either.Validation (validationToEither)
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
import qualified Data.Text.IO as Text (hPutStrLn, putStr, putStrLn)
import Data.Text.Prettyprint.Doc ((<+>), pretty)
import qualified Data.Text.Prettyprint.Doc as Pretty
    ( Doc
    , brackets
    , squotes
    , vsep
    )
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Pretty (AnsiStyle)
import qualified Data.Text.Prettyprint.Doc.Util as Pretty (reflow)
import Dhall (FromDhall)
import qualified Dhall
import qualified Dhall.TH (staticDhallExpression)
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
import qualified System.AtomicWrite.Writer.ByteString as ByteString
    ( atomicWriteFile
    )
import System.AtomicWrite.Writer.String (atomicWriteFile)
import qualified System.AtomicWrite.Writer.Text as Text (atomicWriteFile)
import System.Process (CreateProcess(env), proc, readCreateProcessWithExitCode)
import Text.Fuzzy as Fuzzy (Fuzzy)
import qualified Text.Fuzzy as Fuzzy (Fuzzy(original), filter)

import qualified CommandWrapper.Config.Global as Global (Config(..), getAliases)
import CommandWrapper.Environment (AppNames(AppNames, exePath, usedName))
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
    , hPutExpr
    )
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
import CommandWrapper.Internal.Utils (runMain)
import CommandWrapper.Message (Result, errorMsg, out)
import CommandWrapper.Options.Alias (Alias(alias))
import qualified CommandWrapper.Options.Alias as Options (applyAliasCompletion)
import qualified CommandWrapper.Options.Optparse as Options
    ( bashCompleter
    , internalSubcommandParse
    , splitArguments
    , splitArguments'
    )
import qualified CommandWrapper.Options.Shell as Options


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

--  , addPrefix :: String
--  , addSuffix :: String

    , output :: OutputStdoutOrFile
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
  deriving stock (Generic, Show)

defQueryOptions :: Query
defQueryOptions = Query
    { what = QuerySubcommands
    , patternToMatch = ""
    , matchingAlgorithm = PrefixEquivalence
    , caseSensitive = False
    , output = OutputStdoutOnly
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

data ImportOrContent = Import | Content
  deriving stock (Generic, Show)

data LibraryOptions = LibraryOptions
    { library :: Library
    , output :: OutputStdoutOrFile
    , importOrContent :: ImportOrContent
    }
  deriving stock (Generic, Show)

data Library
    = ShellLibrary Options.Shell
    | DhallLibrary DhallLibrary
  deriving stock (Generic, Show)

data DhallLibrary
    = PreludeV11_1_0
    | PreludeV12_0_0
    | CommandWrapper
    | CommandWrapperExec
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
  deriving anyclass (FromDhall)

type CompletionInfo = Options.Shell -> Natural -> [Text] -> [Text]

instance Options.HasShell CompletionOptions where
    updateShell =
        mapEndo \f opts@CompletionOptions{shell} ->
            (opts :: CompletionOptions){shell = f shell}

instance Options.HasShell ScriptOptions where
    updateShell = mapEndo \f opts@ScriptOptions{shell} ->
        (opts :: ScriptOptions){shell = f shell}

instance Options.HasShell Library where
    updateShell f = Endo \case
        ShellLibrary shell ->
            ShellLibrary (f `appEndo` shell)
        DhallLibrary _ ->
            -- Bash is the default shell.
            ShellLibrary (f `appEndo` Options.Bash)

instance Options.HasShell LibraryOptions where
    updateShell f = Endo \opts@LibraryOptions{library} ->
        (opts :: LibraryOptions)
            { library = Options.updateShell f `appEndo` library
            }

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

defScriptOptions :: ScriptOptions
defScriptOptions = ScriptOptions
    { shell = Options.Bash
    , subcommand = Nothing
    , aliases = []
    , output = OutputStdoutOnly
    }

defLibraryOptions :: LibraryOptions
defLibraryOptions = LibraryOptions
    { library = ShellLibrary Options.Bash
    , output = OutputStdoutOnly
    , importOrContent = Content
    }

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
                >>= outputStringLines output

        -- TODO:
        --
        -- - Completion script should be configurable.
        ScriptMode
          ScriptOptions{shell, aliases, subcommand, output}
          Global.Config{colourOutput, verbosity} -> do

            let mkCompletionScriptExpr =
                    $(Dhall.TH.staticDhallExpression
                        "./dhall/completion.dhall"
                    )

                AppNames{exePath, usedName} = appNames

                script = MkCompletionScript
                    <$> Dhall.extract Dhall.auto mkCompletionScriptExpr

            case validationToEither script of
                Left e -> do
                    -- TODO: Figure out how to handle this better.
                    let subcommand' :: forall ann. Pretty.Doc ann
                        subcommand' = pretty (usedName <> " completion")

                    errorMsg subcommand' verbosity colourOutput stderr
                        ( "Failed to generate completion script: "
                        <> fromString (show e)
                        )
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
                Right (MkCompletionScript mkScript) -> case subcommand of
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

        LibraryMode
          LibraryOptions
            { library = DhallLibrary dhallLib
            , output
            , importOrContent
            }
          config' -> do

            let hPutExpr = Dhall.hPutExpr config'

                hPutLib :: Handle -> IO ()
                hPutLib h = case (dhallLib, importOrContent) of
                    (PreludeV11_1_0, Content) -> hPutExpr h
                        $(Dhall.TH.staticDhallExpression
                            "https://prelude.dhall-lang.org/v12.0.0/package.dhall\
                            \ sha256:aea6817682359ae1939f3a15926b84ad5763c24a3740103202d2eaaea4d01f4c"
                        )

                    (PreludeV11_1_0, Import) -> Text.hPutStrLn h
                        "https://prelude.dhall-lang.org/v12.0.0/package.dhall\
                        \ sha256:aea6817682359ae1939f3a15926b84ad5763c24a3740103202d2eaaea4d01f4c"

                    (PreludeV12_0_0, Content) -> hPutExpr h
                        $(Dhall.TH.staticDhallExpression
                            "https://prelude.dhall-lang.org/v12.0.0/package.dhall\
                            \ sha256:aea6817682359ae1939f3a15926b84ad5763c24a3740103202d2eaaea4d01f4c"
                        )

                    (PreludeV12_0_0, Import) -> Text.hPutStrLn h
                        "https://prelude.dhall-lang.org/v12.0.0/package.dhall\
                        \ sha256:aea6817682359ae1939f3a15926b84ad5763c24a3740103202d2eaaea4d01f4c"

                    -- IMPORTANT!
                    --
                    -- Whenever one of these fails to compile then Command
                    -- Wrapper library was updated, and both hash and import
                    -- need updating.  Unfortunately it won't always fail due
                    -- to caching. Keeping this up to date will be a pain.
                    --
                    -- TODO: Figure out how to do this in a better way.

                    (CommandWrapper, Content) -> hPutExpr h
                        $(Dhall.TH.staticDhallExpression
                            "./dhall/CommandWrapper/package.dhall\
                            \ sha256:f4ef981feb493d9a3a6a956dcad55214a652527663d882e77cead21a38e2416e"
                        )

                    (CommandWrapper, Import) -> Text.hPutStrLn h
                        "https://raw.githubusercontent.com/trskop/command-wrapper/719f7f00b8e01886691127634508ba56be10852a/dhall/CommandWrapper/package.dhall\
                        \ sha256:f4ef981feb493d9a3a6a956dcad55214a652527663d882e77cead21a38e2416e"

                    (CommandWrapperExec, Content) -> hPutExpr h
                        $(Dhall.TH.staticDhallExpression
                            "./dhall/Exec/package.dhall\
                            \ sha256:38900a8210a254861364428b9ab96a1ac473a73b2fc271085d4dda2afc2e9a9c"
                        )

                    (CommandWrapperExec, Import) -> Text.hPutStrLn h
                        "https://raw.githubusercontent.com/trskop/exec/719f7f00b8e01886691127634508ba56be10852a/dhall/Exec/package.dhall\
                        \ sha256:38900a8210a254861364428b9ab96a1ac473a73b2fc271085d4dda2afc2e9a9c"

            case output of
                OutputHandle _ ->
                    hPutLib stdout

                OutputNotHandle (OutputFile filePath) ->
                    -- TODO: Use atomic version instead.
                    withFile filePath WriteMode hPutLib

        LibraryMode
          LibraryOptions{library = ShellLibrary shell, output, importOrContent}
          Global.Config{colourOutput, verbosity} -> do

            let lib :: ByteString
                lib = $(embedFile "bash/lib.bash")

                importScriptExpr =
                    $(Dhall.TH.staticDhallExpression
                        "./dhall/import-shell-library.dhall < Bash >.Bash"
                    )

                subcommand' :: forall ann. Pretty.Doc ann
                subcommand' = pretty (usedName appNames <> " completion")

                getImportScript :: IO Text
                getImportScript = do
                    let script = Dhall.extract Dhall.auto importScriptExpr
                    case validationToEither script of
                        Left e -> do
                            errorMsg subcommand' verbosity colourOutput stderr
                                ( "Failed to generate completion script: "
                                <> fromString (show e)
                                )
                            -- TODO: This is probably not the best exit code.
                            exitWith (ExitFailure 1)

                        Right t ->
                            pure t

            case shell of
                -- TODO: We should consider piping the output to a pager or
                -- similar tool when the stdout is attached to a terminal.  For
                -- example `bat` would be a great choice if it is installed.
                Options.Bash -> do
                    case output of
                        OutputHandle _ ->
                            case importOrContent of
                                Import ->
                                    getImportScript >>= Text.putStr
                                Content ->
                                    ByteString.putStr lib

                        OutputNotHandle (OutputFile fn) ->
                            case importOrContent of
                                Import ->
                                    getImportScript >>= Text.atomicWriteFile fn
                                Content ->
                                    ByteString.atomicWriteFile fn lib

                _ -> do
                    errorMsg subcommand' verbosity colourOutput stderr
                        $ fromString (show shell) <> ": Unsupported SHELL value."
                    exitWith (ExitFailure 125)

        QueryMode query@Query{..} config' -> case what of
            QuerySubcommands
              | null patternToMatch ->
                    getSubcommands appNames config' internalSubcommands
                        >>= outputStringLines output

              | otherwise ->
                    outputStringLines output =<< case matchingAlgorithm of
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
                    outputStringLines output internalSubcommands

              | otherwise ->
                    outputStringLines output case matchingAlgorithm of
                        PrefixEquivalence ->
                            prefixEquivalence query internalSubcommands

                        FuzzyMatch ->
                            Fuzzy.original
                                <$> fuzzyMatch query internalSubcommands

                        Equality ->
                            List.filter (== patternToMatch) internalSubcommands

            QuerySubcommandAliases
              | null patternToMatch ->
                    outputStringLines output (getAliases config')

              | otherwise ->
                    outputStringLines output case matchingAlgorithm of
                        PrefixEquivalence ->
                            findSubcommandAliasesPrefix config' query

                        FuzzyMatch ->
                            Fuzzy.original
                                <$> findSubcommandAliasesFuzzy config' query

                        Equality ->
                            List.filter (== patternToMatch) (getAliases config')

            QueryVerbosityValues
              | null patternToMatch ->
                    outputStringLines output verbosityValues

              | otherwise ->
                    outputStringLines output case matchingAlgorithm of
                        PrefixEquivalence ->
                            prefixEquivalence query verbosityValues

                        FuzzyMatch ->
                            Fuzzy.original <$> fuzzyMatch query verbosityValues

                        Equality ->
                            List.filter (== patternToMatch) verbosityValues

            QueryColourValues
              | null patternToMatch ->
                    outputStringLines output colourValues

              | otherwise ->
                    outputStringLines output case matchingAlgorithm of
                        PrefixEquivalence ->
                            prefixEquivalence query colourValues

                        FuzzyMatch ->
                            Fuzzy.original <$> fuzzyMatch query colourValues

                        Equality ->
                            List.filter (== patternToMatch) colourValues

            QuerySupportedShells
              | null patternToMatch ->
                    outputStringLines output supportedShells

              | otherwise ->
                    outputStringLines output case matchingAlgorithm of
                        PrefixEquivalence ->
                            prefixEquivalence query supportedShells

                        FuzzyMatch ->
                            Fuzzy.original <$> fuzzyMatch query supportedShells

                        Equality ->
                            List.filter (== patternToMatch) supportedShells

            QueryWords words
              | null patternToMatch ->
                    outputStringLines output words

              | otherwise ->
                    outputStringLines output case matchingAlgorithm of
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

    outputStringLines :: OutputStdoutOrFile -> [String] -> IO ()
    outputStringLines = \case
        OutputHandle _ ->
            mapM_ putStrLn

        OutputNotHandle (OutputFile fn) ->
            atomicWriteFile fn . List.unlines

    outputTextLines :: OutputStdoutOrFile -> [Text] -> IO ()
    outputTextLines = \case
        OutputHandle _ ->
            mapM_ Text.putStrLn

        OutputNotHandle (OutputFile fn) ->
            Text.atomicWriteFile fn . Text.unlines

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
                        -- TODO: This requires `bash` to be available.
                        Options.bashCompleter "directory" "--change-directory="
                            pat

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
        -- TODO: This requires `bash` to be available.
        Options.bashCompleter "file" "" pat

  | Just "--output" <- lastBeforePattern =
        -- TODO: This requires `bash` to be available.
        Options.bashCompleter "file" "" pat

  | "--output=" `List.isPrefixOf` pat =
        -- TODO: This requires `bash` to be available.
        Options.bashCompleter "file" "--output=" pat

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
        pure $ List.filter (pat `List.isPrefixOf`) queryOptions

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

    queryOptions =
        ["--subcommands", "--subcommand-aliases", "--supported-shells"
        , "--verbosity-values", "--colour-values", "--color-values"
        , "--pattern=", "--internal-subcommands", "--words"
        ] <> outputOptions <> algorithmOptions

    -- At the moment we have library only for Bash.
    libraryOptions = ["--shell=bash" , "--import" , "--content", "--dhall="]

    dhallLibraryOptions = ("--dhall=" <>)
        <$> [ "prelude"
            , "prelude-v11.1.0"
            , "prelude-v12.0.0"
            , "command-wrapper"
            , "exec"
            ]

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
            <*> optional
                ( updateQueryOptions
                    (   subcommandsFlag
                    <|> internalSubcommandsFlag
                    <|> subcommandAliasesFlag
                    <|> supportedShellsFlag
                    <|> verbosityValuesFlag
                    <|> colourValuesFlag
                    <|> wordsFlag words
                    )
                )
            <*> optional (updateQueryOptions patternOption)
            <*> optional (updateQueryOptions algorithmOption)
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
        parse = Options.eitherReader \s -> case CI.mk s of
            "prelude-v11.1.0" ->
                Right $ Endo \opts ->
                    opts{library = DhallLibrary PreludeV11_1_0}
            "prelude-v12.0.0" ->
                Right $ Endo \opts ->
                    opts{library = DhallLibrary PreludeV12_0_0}
            "prelude" ->
                Right $ Endo \opts ->
                    opts{library = DhallLibrary PreludeV12_0_0}
            "command-wrapper" ->
                Right $ Endo \opts ->
                    opts{library = DhallLibrary CommandWrapper}
            "exec" ->
                Right $ Endo \opts ->
                    opts{library = DhallLibrary CommandWrapperExec}
            _ ->
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
            <+> Pretty.brackets (longOptionWithArgument "output" "FILE")
            <+> Pretty.brackets
                (         longOption "subcommands"
                <> "|" <> longOption "subcommand-aliases"
                <> "|" <> longOption "supported-shells"
                <> "|" <> longOption "verbosity-values"
                <> "|" <> longOption "colo[u]r-values"
                )
            <+> Pretty.brackets (longOptionWithArgument "pattern" "PATTERN")

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

        , optionDescription ["--words [--] [WORD [...]]"]
            [ "Query matching words from", metavar "WORD", "list."
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
            , metavar "LIBRARY", "are"
            , value "prelude" <> ","
            , value "prelude-v11.1.0" <> ","
            , value "prelude-v12.0.0" <> ","
            , value "command-wrapper" <> ",", "and"
            , value "exec" <> "."
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
