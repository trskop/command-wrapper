{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:      $Header$
-- Description: Implementation of internal command named config
-- Copyright:   (c) 2018-2019 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Implementation of internal command named @config@.
module CommandWrapper.Internal.Subcommand.Config
    ( config
    , configSubcommandHelp
    , configSubcommandCompleter
    )
  where

import Prelude (fromIntegral)

import Control.Applicative ((<*>), (<|>), many, optional, pure)
--import Control.Monad ((>>=), unless)
import Data.Bool (Bool(False, True), (||), not, otherwise)
import qualified Data.Char as Char (isDigit)
import Data.Either (Either(Left, Right))
import Data.Foldable (asum, null)
import Data.Function (($), (.), const)
import Data.Functor (Functor, (<$>), (<&>))
import qualified Data.List as List
    ( any
    , dropWhile
    , elem
    , filter
    , isPrefixOf
    , or
    , take
    )
--import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Maybe (Maybe(Just), fromMaybe)
import Data.Monoid (Endo(Endo, appEndo), (<>), mconcat, mempty)
import Data.String (String)
import Data.Word (Word)
import GHC.Generics (Generic)
--import System.Exit (ExitCode(ExitFailure), exitWith)
import System.IO (IO{-, stderr-}, stdout)
import Text.Show (Show)

import Data.CaseInsensitive as CI (mk)
import Data.Monoid.Endo.Fold (dualFoldEndo)
import Data.Output (HasOutput(Output), setOutput)
import Data.Text (Text)
import Data.Text.Prettyprint.Doc ((<+>){-, pretty-})
import qualified Data.Text.Prettyprint.Doc as Pretty
    ( Doc
    , brackets
--  , hsep
--  , line
    , squotes
    , vsep
    )
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Pretty (AnsiStyle)
import qualified Data.Text.Prettyprint.Doc.Util as Pretty (reflow)
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
    , strArgument
    , strOption
    )
import qualified Options.Applicative.Standard as Options (outputOption)
import Safe (atMay, lastMay)

import qualified CommandWrapper.Config.Global as Global (Config(..))
import CommandWrapper.Environment (AppNames(AppNames, usedName))
import CommandWrapper.Internal.Subcommand.Help
    ( globalOptionsHelp
    , helpOptions
    , longOption
    , longOptionWithArgument
    , metavar
    , optionDescription
--  , optionalMetavar
    , section
    , toolsetCommand
    , usageSection
    , value
    )
import CommandWrapper.Internal.Utils (runMain)
import CommandWrapper.Message
    ( Result
    , defaultLayoutOptions
--  , errorMsg
    , message
    )
import qualified CommandWrapper.Options.Optparse as Options
    ( bashCompleter
    , internalSubcommandParse
--  , splitArguments
--  , splitArguments'
    )
import qualified CommandWrapper.Options.Shell as Options (Shell)
import CommandWrapper.Internal.Subcommand.Config.Dhall
    ( HasInput
    , parseInput
    , setInput
    )
import qualified CommandWrapper.Internal.Subcommand.Config.Dhall as Dhall
    ( Diff(..)
    , Format(..)
    , Freeze(..)
    , Interpreter(..)
    , Lint(..)
    , Output(..)
    , Repl(..)
    , Resolve(..)
    , ResolveMode(..)
    , defDiff
    , defFormat
    , defFreeze
    , defInterpreter
    , defLint
    , defRepl
    , defResolve
    , diff
    , format
    , freeze
    , hash
    , interpreter
    , lint
    , repl
    , resolve
    )
import CommandWrapper.Internal.Subcommand.Config.Init
    ( InitOptions(..)
    , defInitOptions
    , init
    )


data ConfigMode a
    = Init InitOptions a
    | ConfigLib a
    | Dhall Dhall.Interpreter a
    | DhallDiff Dhall.Diff a
    | DhallFormat Dhall.Format a
    | DhallFreeze Dhall.Freeze a
    | DhallHash a
    | DhallLint Dhall.Lint a
    | DhallRepl Dhall.Repl a
    | DhallResolve Dhall.Resolve a
    | Help a
  deriving (Functor, Generic, Show)

config :: AppNames -> [String] -> Global.Config -> IO ()
config appNames options globalConfig =
    runMain (parseOptions appNames globalConfig options) defaults \case
        Init opts cfg ->
            init appNames cfg opts

        ConfigLib _ -> pure ()

        Help config'@Global.Config{colourOutput, verbosity} ->
            message defaultLayoutOptions verbosity colourOutput stdout
                (configSubcommandHelp appNames config')

        -- TODO:
        --
        -- - Merge in functionality of: `dhall`, `dhall-json`, `dhall-bash`,
        --   and `dhall-text`
        -- - Provide functionality for shell variables that transforms:
        --
        --     ```
        --     { name = "FOO"
        --     , value = "foo"
        --     }
        --     ```
        --
        --     Into:
        --
        --     ```
        --     export FOO=foo
        --     ```
        --
        --     It should also support transorming:
        --
        --     ```
        --     [ { name = "FOO"
        --       , value = "foo"
        --       }
        --     , { name = "BAR"
        --       , value = "bar"
        --       }
        --     ]
        --     ```
        --
        --     Into:
        --
        --     ```
        --     export FOO=foo
        --     export FOO=bar
        --     ```
        Dhall opts cfg ->
            Dhall.interpreter appNames cfg opts

        DhallDiff diffOpts cfg ->
            Dhall.diff appNames cfg diffOpts

        DhallFormat formatOpts cfg ->
            Dhall.format appNames cfg formatOpts

        DhallFreeze freezeOpts cfg ->
            Dhall.freeze appNames cfg freezeOpts

        DhallHash cfg ->
            Dhall.hash appNames cfg

        DhallLint lintOpts cfg ->
            Dhall.lint appNames cfg lintOpts

        DhallRepl replOpts cfg ->
            Dhall.repl appNames cfg replOpts

        DhallResolve resolveOpts cfg ->
            Dhall.resolve appNames cfg resolveOpts
  where
    defaults = Mainplate.applySimpleDefaults (Help globalConfig)

    {-
    dieWith :: Global.Config -> Int -> (forall ann. Pretty.Doc ann) -> IO a
    dieWith cfg exitCode msg = do
        let Global.Config{colourOutput, verbosity} = cfg

            subcommand :: forall ann. Pretty.Doc ann
            subcommand = pretty (usedName <> " config")

        errorMsg subcommand verbosity colourOutput stderr msg
        exitWith (ExitFailure exitCode)

    messageLn Global.Config{colourOutput, verbosity} fragments =
        message defaultLayoutOptions verbosity colourOutput stdout
            (Pretty.hsep fragments <> Pretty.line)
    -}

parseOptions
    :: AppNames
    -> Global.Config
    -> [String]
    -> IO (Endo (ConfigMode Global.Config))
parseOptions appNames@AppNames{usedName} globalConfig options = execParser
    [ dualFoldEndo
        <$> initFlag
        <*> optional toolsetOption

    , dhallFlag
        <*> ( dualFoldEndo
                <$> many (allowImportsFlag <|> noAllowImportsFlag)
                <*> many (alphaFlag <|> noAlphaFlag)
                <*> many (annotateFlag <|> noAnnotateFlag)
                <*> many (typeFlag <|> noTypeFlag)
                <*> optional outputOption
                <*> optional inputOption
            )

    , dhallReplFlag

    , dhallDiffFlag
        <*> Options.strArgument mempty
        <*> Options.strArgument mempty
        <*> ( dualFoldEndo
                <$> optional outputOption
            )

    , dhallHashFlag

    , dhallFreezeFlag
        <*> ( dualFoldEndo
                <$> many (remoteOnlyFlag <|> noRemoteOnlyFlag)
                <*> optional outputOption
                <*> optional inputOption
            )

    , dhallFormatFlag
        <*> pure mempty
--      <*> (checkFlag <|> noCheckFlag)

    , dhallLintFlag
        <*> ( dualFoldEndo
                <$> optional outputOption
                <*> optional inputOption
            )

    , dhallResolveFlag
        <*> ( dualFoldEndo
                <$> optional outputOption
                <*> optional inputOption
                <*> optional listDependenciesOption
            )

    , helpFlag

    , pure mempty
    ]
  where
    switchTo :: ConfigMode Global.Config -> Endo (ConfigMode Global.Config)
    switchTo = Endo . const

    switchToHelpMode, switchToInitMode, switchToDhallHashMode,
        switchToDhallReplMode :: Endo (ConfigMode Global.Config)

    switchToHelpMode = switchTo (Help globalConfig)
    switchToInitMode = switchTo (Init (defInitOptions usedName) globalConfig)
    switchToDhallHashMode = switchTo (DhallHash globalConfig)
    switchToDhallReplMode = switchTo (DhallRepl Dhall.defRepl globalConfig)

    switchToDhallMode f =
        switchTo (Dhall (f `appEndo` Dhall.defInterpreter) globalConfig)

    switchToDhallDiffMode
        :: Text
        -> Text
        -> Endo Dhall.Diff
        -> Endo (ConfigMode Global.Config)
    switchToDhallDiffMode expr1 expr2 f =
        switchTo (DhallDiff (f `appEndo` Dhall.defDiff expr1 expr2) globalConfig)

    switchToDhallFreezeMode
        :: Endo Dhall.Freeze
        -> Endo (ConfigMode Global.Config)
    switchToDhallFreezeMode f =
        switchTo (DhallFreeze (f `appEndo` Dhall.defFreeze) globalConfig)

    switchToDhallFormatMode
        :: Endo Dhall.Format
        -> Endo (ConfigMode Global.Config)
    switchToDhallFormatMode f =
        switchTo (DhallFormat (f `appEndo` Dhall.defFormat) globalConfig)

    switchToDhallLintMode :: Endo Dhall.Lint -> Endo (ConfigMode Global.Config)
    switchToDhallLintMode f =
        switchTo (DhallLint (f `appEndo` Dhall.defLint) globalConfig)

    switchToDhallResolveMode
        :: Endo Dhall.Resolve
        -> Endo (ConfigMode Global.Config)
    switchToDhallResolveMode f =
        switchTo (DhallResolve (f `appEndo` Dhall.defResolve) globalConfig)

    initFlag :: Options.Parser (Endo (ConfigMode Global.Config))
    initFlag = Options.flag mempty switchToInitMode (Options.long "init")

    dhallFlag
        :: Options.Parser
            (Endo Dhall.Interpreter -> Endo (ConfigMode Global.Config))
    dhallFlag =
        Options.flag mempty switchToDhallMode (Options.long "dhall")

    alphaFlag :: Options.Parser (Endo Dhall.Interpreter)
    alphaFlag = Options.flag' (setAlpha True) (Options.long "alpha")

    noAlphaFlag :: Options.Parser (Endo Dhall.Interpreter)
    noAlphaFlag = Options.flag' (setAlpha False) (Options.long "no-alpha")

    setAlpha :: Bool -> Endo Dhall.Interpreter
    setAlpha alpha = Endo \opts -> opts{Dhall.alpha}

    annotateFlag :: Options.Parser (Endo Dhall.Interpreter)
    annotateFlag = Options.flag' (setAnnotate True) (Options.long "annotate")

    noAnnotateFlag :: Options.Parser (Endo Dhall.Interpreter)
    noAnnotateFlag =
        Options.flag' (setAnnotate False) (Options.long "no-annotate")

    setAnnotate :: Bool -> Endo Dhall.Interpreter
    setAnnotate annotate = Endo \opts -> opts{Dhall.annotate}

    allowImportsFlag :: Options.Parser (Endo Dhall.Interpreter)
    allowImportsFlag =
        Options.flag' (setAllowImports True) (Options.long "allow-imports")

    noAllowImportsFlag :: Options.Parser (Endo Dhall.Interpreter)
    noAllowImportsFlag =
        Options.flag' (setAllowImports False) (Options.long "no-allow-imports")

    setAllowImports :: Bool -> Endo Dhall.Interpreter
    setAllowImports allowImports = Endo \opts -> opts{Dhall.allowImports}

    typeFlag :: Options.Parser (Endo Dhall.Interpreter)
    typeFlag = Options.flag' (setType True) (Options.long "type")

    noTypeFlag :: Options.Parser (Endo Dhall.Interpreter)
    noTypeFlag = Options.flag' (setType False) (Options.long "no-type")

    setType :: Bool -> Endo Dhall.Interpreter
    setType showType = Endo \opts -> opts{Dhall.showType}

    inputOption
        :: HasInput a
        => Options.Parser (Endo a)
    inputOption =
        Endo . setInput
            <$> Options.option (Options.eitherReader parseInput)
                    (Options.long "input" <> Options.short 'i')

    outputOption
        :: (Output a ~ Dhall.Output, HasOutput a)
        => Options.Parser (Endo a)
    outputOption = Endo . setOutput <$> Options.outputOption

    dhallDiffFlag
        :: Options.Parser
            (Text -> Text -> Endo Dhall.Diff -> Endo (ConfigMode Global.Config))
    dhallDiffFlag =
        Options.flag mempty switchToDhallDiffMode (Options.long "dhall-diff")

    dhallFreezeFlag
        :: Options.Parser (Endo Dhall.Freeze -> Endo (ConfigMode Global.Config))
    dhallFreezeFlag =
        Options.flag mempty switchToDhallFreezeMode
            (Options.long "dhall-freeze")

    remoteOnlyFlag :: Options.Parser (Endo Dhall.Freeze)
    remoteOnlyFlag =
        Options.flag' (setRemoteOnly True) (Options.long "remote-only")

    noRemoteOnlyFlag :: Options.Parser (Endo Dhall.Freeze)
    noRemoteOnlyFlag =
        Options.flag' (setRemoteOnly False) (Options.long "no-remote-only")

    setRemoteOnly :: Bool -> Endo Dhall.Freeze
    setRemoteOnly remoteOnly = Endo \opts -> opts{Dhall.remoteOnly}

    dhallHashFlag :: Options.Parser (Endo (ConfigMode Global.Config))
    dhallHashFlag =
        Options.flag mempty switchToDhallHashMode (Options.long "dhall-hash")

    dhallReplFlag :: Options.Parser (Endo (ConfigMode Global.Config))
    dhallReplFlag =
        Options.flag mempty switchToDhallReplMode (Options.long "dhall-repl")

    dhallFormatFlag
        :: Options.Parser (Endo Dhall.Format -> Endo (ConfigMode Global.Config))
    dhallFormatFlag =
        Options.flag mempty switchToDhallFormatMode
            (Options.long "dhall-format")

    dhallLintFlag
        :: Options.Parser (Endo Dhall.Lint -> Endo (ConfigMode Global.Config))
    dhallLintFlag =
        Options.flag mempty switchToDhallLintMode
            (Options.long "dhall-lint")

    dhallResolveFlag
        :: Options.Parser
            (Endo Dhall.Resolve -> Endo (ConfigMode Global.Config))
    dhallResolveFlag =
        Options.flag mempty switchToDhallResolveMode
            (Options.long "dhall-resolve")

    listDependenciesOption :: Options.Parser (Endo Dhall.Resolve)
    listDependenciesOption =
        Options.option parser
            (Options.long "list-imports" <> Options.metavar "KIND")
      where
        parser = Options.eitherReader \s -> case CI.mk s of
            "immediate" -> Right $ Endo \r ->
                r{Dhall.mode = Dhall.ListImmediateDependencies}

            "transitive" -> Right $ Endo \r ->
                r{Dhall.mode = Dhall.ListTransitiveDependencies}

            _ -> Left "Expected 'immediate' or 'transitive'"

    toolsetOption :: Options.Parser (Endo (ConfigMode Global.Config))
    toolsetOption =
        Options.strOption (Options.long "toolset" <> Options.metavar "NAME")
            <&> \toolsetName -> Endo \case
                    Init opts cfg ->
                        Init (opts :: InitOptions){toolsetName} cfg
                    mode ->
                        mode

    helpFlag :: Options.Parser (Endo (ConfigMode Global.Config))
    helpFlag = Options.flag mempty switchToHelpMode $ mconcat
        [ Options.short 'h'
        , Options.long "help"
        ]

    execParser
        :: [Options.Parser (Endo (ConfigMode Global.Config))]
        -> IO (Endo (ConfigMode Global.Config))
    execParser parser =
        Options.internalSubcommandParse appNames globalConfig "config"
            Options.defaultPrefs (Options.info (asum parser) mempty) options

configSubcommandHelp
    :: AppNames
    -> Global.Config
    -> Pretty.Doc (Result Pretty.AnsiStyle)
configSubcommandHelp AppNames{usedName} _config = Pretty.vsep
    [ Pretty.reflow
        "Initialise, query, and update Command Wrapper toolset configuration."
    , ""

    , usageSection usedName
--      [ "config" <+> optionalMetavar "EXPRESSION"
--
        [ "config"
            <+> longOption "dhall"
            <+> Pretty.brackets
                    ( longOption "[no-]alpha"
                    <> "|" <> longOption "[no-]allow-imports"
                    <> "|" <> longOption "[no-]annotate"
                    <> "|" <> longOption "[no-]type"
                    )
            <+> Pretty.brackets (longOptionWithArgument "input" "FILE")
            <+> Pretty.brackets (longOptionWithArgument "output" "FILE")

        , "config"
            <+> longOption "dhall-format"
--          <+> Pretty.brackets (longOption "[no-]check")
--          <+> Pretty.brackets (longOptionWithArgument "input" "FILE")
--          <+> Pretty.brackets (longOptionWithArgument "output" "FILE")

        , "config"
            <+> longOption "dhall-lint"
            <+> Pretty.brackets (longOptionWithArgument "input" "FILE")
            <+> Pretty.brackets (longOptionWithArgument "output" "FILE")

        , "config"
            <+> longOption "dhall-resolve"
            <+> Pretty.brackets (longOptionWithArgument "list-imports" "KIND")
            <+> Pretty.brackets (longOptionWithArgument "input" "FILE")
            <+> Pretty.brackets (longOptionWithArgument "output" "FILE")

        , "config"
            <+> longOption "dhall-freeze"
            <+> Pretty.brackets (longOption "[no-]remote-only")
            <+> Pretty.brackets (longOptionWithArgument "input" "FILE")
            <+> Pretty.brackets (longOptionWithArgument "output" "FILE")

        , "config"
            <+> longOption "dhall-hash"
--          <+> Pretty.brackets (longOptionWithArgument "input" "FILE")
--          <+> Pretty.brackets (longOptionWithArgument "output" "FILE")

{-          <+> Pretty.brackets
                    -- cbor, cbor-json, json, text,
                    ( longOptionWithArgument "input-encoding" "INPUT_ENCODING"
                    <> "|" <> longOptionWithArgument "output-encoding" "OUTPUT_ENCODING"
                    )
-}
        , "config"
            <+> longOption "dhall-diff"
            <+> metavar "EXPRESSION"
            <+> metavar "EXPRESSION"
            <+> Pretty.brackets (longOptionWithArgument "output" "FILE")

        , "config"
            <+> longOption "dhall-repl"
--          <+> Pretty.brackets
--                  -- TODO: Consider having COMMAND_WRAPPER_DHALL_REPL_HISTORY
--                  -- environment variable as well.
--                  ( longOptionWithArgument "history-file" "FILE"
--                  <> "|" <> longOption "no-history-file"
--                  )

        , "config"
            <+> longOption "init"
            <+> longOptionWithArgument "toolset" "NAME"

        , "config" <+> helpOptions
        , "help config"
        ]

    , section "Options:"
        [ optionDescription ["--dhall"]
            [ Pretty.reflow "Run as interpreter for the Dhall language."
            ]

        , optionDescription ["--[no-]allow-imports"]
            [ Pretty.reflow "Controls whether imports in the input expression\
                \ are allowed or not. By default imports are allowed."
            ]

        , optionDescription ["--[no-]alpha"]
            [ Pretty.reflow "Perform α-normalisation of Dhall expression. By\
                \ default α-normalisation is not performed."
            ]

        , optionDescription ["--[no-]annotate"]
            [ Pretty.reflow "Add a type annotation to the output. Type\
                \ annotations aren't included by default."
            ]

        , optionDescription ["--input=FILE", "--input FILE", "-i FILE"]
            [ Pretty.reflow "Read input from", metavar "FILE"
            , Pretty.reflow "instead of standard input."
            ]

        , optionDescription ["--output=FILE", "--output FILE", "-o FILE"]
            [ Pretty.reflow "Write optput into", metavar "FILE"
            , Pretty.reflow "instead of standard output."
            ]

        , optionDescription ["--dhall-format"]
            [ Pretty.reflow "Format Dhall expression."
            ]

        , optionDescription ["--dhall-lint"]
            [ Pretty.reflow "Dhall linter; improve Dhall expression."
            ]

        , optionDescription ["--dhall-resolve"]
            [ Pretty.reflow "Resolve an Dhall expression's imports."
            ]

        , optionDescription ["--list-imports=KIND"]
            [ Pretty.reflow "Instead of resolving imports list them one on\
                \ each line."
            , metavar "KIND", "can" <+> "be", value "immediate", "or"
            , value "transitive" <> "."
            ]

--      , optionDescription ["--[no-]check"]
--          [ Pretty.reflow "If enabled it only checks if the input is\
--              \ formatted."
--          ]

        , optionDescription ["--dhall-freeze"]
            [ Pretty.reflow "Add integrity checks to import statements of a\
                \ Dhall expression."
            ]

        , optionDescription ["--[no-]remote-only"]
            [ Pretty.reflow "Specifies if integrity checks should be added to\
                \ only remote imports or to all imports. By default they are\
                \ added only to remote imports."
            ]

        , optionDescription ["--dhall-hash"]
            [ Pretty.reflow "Compute semantic hashes for Dhall expressions."
            ]

        , optionDescription ["--dhall-diff"]
            [ Pretty.reflow "Render the difference between the normal form of\
                \ two Dhall expressions."
            ]

        , optionDescription ["--dhall-repl"]
            [ Pretty.reflow "Interpret Dhall expressions in a REPL."
            ]

--      , optionDescription ["--history-file=FILE"]
--          [ Pretty.reflow "TODO"
--          ]

--      , optionDescription ["--no-history-file"]
--          [ Pretty.reflow "TODO"
--          ]

        , optionDescription ["--init"]
            [ Pretty.reflow "Initialise configuration of a toolset."
            ]

        , optionDescription ["--toolset=NAME"]
            [ Pretty.reflow "When specified allong with", longOption "init"
            , Pretty.reflow "then configuration for toolset", metavar "NAME"
            , Pretty.reflow "is initialised."
            ]

        , optionDescription ["--help", "-h"]
            [ Pretty.reflow "Print this help and exit. Same as"
            , Pretty.squotes (toolsetCommand usedName "help config") <> "."
            ]

        , optionDescription ["EPRESSION"]
            [ "Dhall", metavar "EXPRESSION"
            , Pretty.reflow "that will be applied to configuration."
            ]

        , globalOptionsHelp usedName
        ]

    , ""
    ]

configSubcommandCompleter
    :: AppNames
    -> Global.Config
    -> Options.Shell
    -> Word
    -> [String]
    -> IO [String]
configSubcommandCompleter _appNames _cfg _shell index words
  | Just "-o" <- lastMay wordsBeforePattern =
        bashCompleter "file" ""

  | Just "--output" <- lastMay wordsBeforePattern =
        bashCompleter "file" ""

  -- TODO: This may be Bash-scpecific.  We need to investigate other shells.
  | Just w <- lastMay wordsBeforePattern, isBashRedirection w =
        bashCompleter "file" ""

  | "--output=" `List.isPrefixOf` pat =
        bashCompleter "file" "--output="

  | null pat =
        pure possibleOptions

  | otherwise =
        pure matchingOptions
  where
    wordsBeforePattern = List.take (fromIntegral index) words
    pat = fromMaybe "" $ atMay words (fromIntegral index)

    hadHelp =
        ("--help" `List.elem` wordsBeforePattern)
        || ("-h" `List.elem` wordsBeforePattern)

    hadInit = "--init" `List.elem` wordsBeforePattern

    hadDhall = "--dhall" `List.elem` wordsBeforePattern
    hadDhallDiff = "--dhall-diff" `List.elem` wordsBeforePattern
    hadDhallFormat = "--dhall-format" `List.elem` wordsBeforePattern
    hadDhallFreeze = "--dhall-freeze" `List.elem` wordsBeforePattern
    hadDhallHash = "--dhall-hash" `List.elem` wordsBeforePattern
    hadDhallLint = "--dhall-lint" `List.elem` wordsBeforePattern
    hadDhallRepl = "--dhall-repl" `List.elem` wordsBeforePattern
    hadDhallResolve = "--dhall-resolve" `List.elem` wordsBeforePattern

    hadSomeDhall = List.or
        [ hadDhall
        , hadDhallDiff
        , hadDhallFormat
        , hadDhallFreeze
        , hadDhallHash
        , hadDhallLint
        , hadDhallRepl
        , hadDhallResolve
        ]

    hadOutput = List.or
        [ "-o" `List.elem` wordsBeforePattern
        , "--output" `List.elem` wordsBeforePattern
        , List.any ("--output=" `List.isPrefixOf`) wordsBeforePattern
        ]

    mwhen p x = if p then x else mempty
    munless p = mwhen (not p)

    possibleOptions =
        munless (hadHelp || hadInit || hadSomeDhall)
            [ "--help", "-h"
            , "--init"

            , "--dhall", "--dhall-diff", "--dhall-format", "--dhall-freeze"
            , "--dhall-hash", "--dhall-lint", "--dhall-repl", "--dhall-resolve"
            ]
        <> munless (hadHelp || hadSomeDhall || not hadInit) ["--toolset="]
        <> munless
            ( List.or
                [ hadHelp, not hadDhall, hadDhallDiff, hadDhallFreeze
                , hadDhallFormat, hadDhallHash, hadDhallLint, hadDhallRepl
                , hadDhallResolve, hadInit
                ]
            )
            [ "--alpha", "--no-alpha"
            , "--allow-imports", "--no-allow-imports"
            , "--annotate", "--no-annotate"
            , "--type", "--no-type"
            ]
        <> munless
            ( List.or
                [ hadHelp
                , not $ List.or
                    [ hadDhall
                    , hadDhallDiff
                    , hadDhallFreeze
                    , hadDhallLint
                    , hadDhallResolve
                    ]
                , hadDhallFormat, hadDhallHash, hadDhallRepl, hadInit
                , hadOutput -- Output options can be specified only once.
                ]
            )
            ["-o", "--output="]
        <> munless
            ( List.or
                [ hadHelp, hadDhall, hadDhallDiff, not hadDhallFreeze
                , hadDhallFormat, hadDhallHash, hadDhallRepl, hadDhallResolve
                , hadInit
                ]
            )
            ["--remote-only", "--no-remote-only"]
--      <> munless
--          ( List.or
--              [ hadHelp, hadDhall, hadDhallDiff, hadDhallFreeze
--              , not hadDhallFormat, hadDhallHash, hadDhallRepl
--              , hadDhallResolve, hadInit
--              ]
--          )
--          ["--check", "--no-check"]
        <> munless
            ( List.or
                [ hadHelp, hadDhall, hadDhallDiff, hadDhallFreeze
                , hadDhallFormat, hadDhallHash, hadDhallRepl
                , not hadDhallResolve, hadInit
                ]
            )
            ["--list-imports=immediate", "--list-imports=transitive"]

    matchingOptions = List.filter (pat `List.isPrefixOf`) possibleOptions

    -- See `bash(1)` for details.
    isBashRedirection s = List.dropWhile Char.isDigit s `List.elem`
        [ "&>"
        , "&>>"
        , "<"
        , "<>"
        , ">"
        , ">&"
        , ">>"
        ]

    bashCompleter a p = Options.bashCompleter a p pat
