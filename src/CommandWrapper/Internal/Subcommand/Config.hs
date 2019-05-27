{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
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
    ( ConfigMode(..)
    , config
    , configSubcommandHelp
    , configCompletion
    )
  where

import Control.Applicative ((<*>), (<|>), optional, pure)
--import Control.Monad ((>>=), unless)
import Data.Bool (Bool(False, True), (||), not, otherwise)
--import Data.Either (Either(Left, Right))
import Data.Foldable (asum, length, null)
import Data.Function (($), (.), const)
import Data.Functor (Functor, (<$>), (<&>), fmap)
import qualified Data.List as List (drop, elem, filter, isPrefixOf, or)
--import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Maybe (Maybe(Just), fromMaybe)
import Data.Monoid (Endo(Endo, appEndo), (<>), mconcat, mempty)
import Data.String (String)
import GHC.Generics (Generic)
--import System.Exit (ExitCode(ExitFailure), exitWith)
import System.IO (IO{-, stderr-}, stdout)
import Text.Show (Show)

import Data.Output (HasOutput(Output), setOutput)
import Data.Monoid.Endo.Fold (dualFoldEndo)
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
    , flag
    , info
    , long
    , metavar
    , short
    , strArgument
    , strOption
    )
import qualified Options.Applicative.Builder.Completer as Options (bashCompleter)
import qualified Options.Applicative.Standard as Options (outputOption)
import qualified Options.Applicative.Types as Options (Completer(runCompleter))
import Safe (lastMay)

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
    )
import CommandWrapper.Internal.Utils (runMain)
import CommandWrapper.Message
    ( Result
    , defaultLayoutOptions
--  , errorMsg
    , message
    )
import qualified CommandWrapper.Options.Optparse as Options
    ( internalSubcommandParse
--  , splitArguments
--  , splitArguments'
    )
import qualified CommandWrapper.Options.ColourOutput as ColourOutput
    ( ColourOutput(Auto)
    )

import qualified CommandWrapper.Internal.Subcommand.Config.Dhall as Dhall
    ( Diff(..)
    , Format(..)
    , Freeze(..)
    , Interpreter(..)
    , Output(..)
    , Repl(..)
    , Resolve(..)
--  , ResolveMode(..)
    , defDiff
    , defFormat
    , defFreeze
    , defInterpreter
    , defRepl
    , defResolve
    , diff
    , format
    , freeze
    , hash
    , interpreter
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

        Help Global.Config{colourOutput, verbosity} ->
            message defaultLayoutOptions verbosity
                (fromMaybe ColourOutput.Auto colourOutput) stdout
                (configSubcommandHelp appNames)

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
            colour = fromMaybe ColourOutput.Auto colourOutput

            subcommand :: forall ann. Pretty.Doc ann
            subcommand = pretty (usedName <> " config")

        errorMsg subcommand verbosity colour stderr msg
        exitWith (ExitFailure exitCode)

    messageLn Global.Config{colourOutput, verbosity} fragments =
        let colourOutput' = fromMaybe ColourOutput.Auto colourOutput

         in message defaultLayoutOptions verbosity colourOutput' stdout
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
                <$> (allowImportsFlag <|> noAllowImportsFlag)
                <*> (alphaFlag <|> noAlphaFlag)
                <*> (annotateFlag <|> noAnnotateFlag)
                <*> (typeFlag <|> noTypeFlag)
            )

    , dhallReplFlag

    , dhallDiffFlag
        <*> Options.strArgument mempty
        <*> Options.strArgument mempty
        <*> ( dualFoldEndo
                <$> optional (outputOption @Dhall.Diff)
            )

    , dhallHashFlag

    , dhallFreezeFlag
        <*> ( dualFoldEndo
                <$> (remoteOnlyFlag <|> noRemoteOnlyFlag)
                <*> optional outputOption
            )

    , dhallFormatFlag
        <*> pure mempty
--      <*> (checkFlag <|> noCheckFlag)

    , dhallResolveFlag
        <*> ( dualFoldEndo
                <$> optional outputOption
--              <*> optional listDependenciesOption
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
    alphaFlag = Options.flag mempty (setAlpha True) (Options.long "alpha")

    noAlphaFlag :: Options.Parser (Endo Dhall.Interpreter)
    noAlphaFlag = Options.flag mempty (setAlpha False) (Options.long "no-alpha")

    setAlpha :: Bool -> Endo Dhall.Interpreter
    setAlpha alpha = Endo \opts -> opts{Dhall.alpha}

    annotateFlag :: Options.Parser (Endo Dhall.Interpreter)
    annotateFlag =
        Options.flag mempty (setAnnotate True) (Options.long "annotate")

    noAnnotateFlag :: Options.Parser (Endo Dhall.Interpreter)
    noAnnotateFlag =
        Options.flag mempty (setAnnotate False) (Options.long "no-annotate")

    setAnnotate :: Bool -> Endo Dhall.Interpreter
    setAnnotate annotate = Endo \opts -> opts{Dhall.annotate}

    allowImportsFlag :: Options.Parser (Endo Dhall.Interpreter)
    allowImportsFlag =
        Options.flag mempty (setAllowImports True)
            (Options.long "allow-imports")

    noAllowImportsFlag :: Options.Parser (Endo Dhall.Interpreter)
    noAllowImportsFlag =
        Options.flag mempty (setAllowImports False)
            (Options.long "no-allow-imports")

    setAllowImports :: Bool -> Endo Dhall.Interpreter
    setAllowImports allowImports = Endo \opts -> opts{Dhall.allowImports}

    typeFlag :: Options.Parser (Endo Dhall.Interpreter)
    typeFlag =
        Options.flag mempty (setType True) (Options.long "type")

    noTypeFlag :: Options.Parser (Endo Dhall.Interpreter)
    noTypeFlag =
        Options.flag mempty (setType False) (Options.long "no-type")

    setType :: Bool -> Endo Dhall.Interpreter
    setType showType = Endo \opts -> opts{Dhall.showType}

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
        Options.flag mempty (setRemoteOnly True) (Options.long "remote-only")

    noRemoteOnlyFlag :: Options.Parser (Endo Dhall.Freeze)
    noRemoteOnlyFlag =
        Options.flag mempty (setRemoteOnly False)
            (Options.long "no-remote-only")

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

    dhallResolveFlag
        :: Options.Parser
            (Endo Dhall.Resolve -> Endo (ConfigMode Global.Config))
    dhallResolveFlag =
        Options.flag mempty switchToDhallResolveMode
            (Options.long "dhall-resolve")

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

configSubcommandHelp :: AppNames -> Pretty.Doc (Result Pretty.AnsiStyle)
configSubcommandHelp AppNames{usedName} = Pretty.vsep
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
--          <+> Pretty.brackets (longOptionWithArgument "input" "FILE")
            <+> Pretty.brackets (longOptionWithArgument "output" "FILE")

        , "config"
            <+> longOption "dhall-format"
--          <+> Pretty.brackets (longOption "[no-]check")
--          <+> Pretty.brackets (longOptionWithArgument "input" "FILE")
--          <+> Pretty.brackets (longOptionWithArgument "output" "FILE")

--      , "config"
--          <+> longOption "dhall-lint"
--          <+> Pretty.brackets (longOptionWithArgument "input" "FILE")
--          <+> Pretty.brackets (longOptionWithArgument "output" "FILE")

        , "config"
            <+> longOption "dhall-resolve"
--          <+> Pretty.brackets (longOptionWithArgument "list" "WHAT")
--          <+> Pretty.brackets (longOptionWithArgument "input" "FILE")
            <+> Pretty.brackets (longOptionWithArgument "output" "FILE")

        , "config"
            <+> longOption "dhall-freeze"
            <+> Pretty.brackets (longOption "[no-]remote-only")
--          <+> Pretty.brackets (longOptionWithArgument "input" "FILE")
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

        , optionDescription ["--output=FILE", "--output FILE", "-o FILE"]
            [ Pretty.reflow "Write optput into", metavar "FILE"
            , Pretty.reflow "instead of standard output."
            ]

        , optionDescription ["--dhall-format"]
            [ Pretty.reflow "Format Dhall expression."
            ]

        , optionDescription ["--dhall-resolve"]
            [ Pretty.reflow "Resolve an Dhall expression's imports."
            ]

--      , optionDescription ["--[no-]check"]
--          [ Pretty.reflow "If enabled it only checks if the input is\
--              \ formatted."
--          ]

--      , optionDescription ["--dhall-lint"]
--          [ Pretty.reflow "Dhall linter; improve Dhall expression."
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

configCompletion
    :: AppNames
    -> Global.Config
    -> [String]
    -> String
    -> IO [String]
configCompletion _appNames _config wordsBeforePattern pat
  | Just "-o" <- lastMay wordsBeforePattern =
        bashCompleter "file" ""

  | Just "--output" <- lastMay wordsBeforePattern =
        bashCompleter "file" ""

  | "--output=" `List.isPrefixOf` pat =
        bashCompleter "file" "--output="

  | null pat =
        pure possibleOptions

  | otherwise =
        pure matchingOptions
  where
    hadHelp =
        ("--help" `List.elem` wordsBeforePattern)
        || ("-h" `List.elem` wordsBeforePattern)

    hadInit = "--init" `List.elem` wordsBeforePattern

    hadDhall = "--dhall" `List.elem` wordsBeforePattern
    hadDhallDiff = "--dhall-diff" `List.elem` wordsBeforePattern
    hadDhallFormat = "--dhall-format" `List.elem` wordsBeforePattern
    hadDhallFreeze = "--dhall-freeze" `List.elem` wordsBeforePattern
    hadDhallHash = "--dhall-hash" `List.elem` wordsBeforePattern
    hadDhallRepl = "--dhall-repl" `List.elem` wordsBeforePattern
    hadDhallResolve = "--dhall-resolve" `List.elem` wordsBeforePattern

    hadSomeDhall = List.or
        [ hadDhall
        , hadDhallDiff
        , hadDhallFormat
        , hadDhallFreeze
        , hadDhallHash
        , hadDhallRepl
        , hadDhallResolve
        ]

    mwhen p x = if p then x else mempty
    munless p = mwhen (not p)

    possibleOptions =
        munless (hadHelp || hadInit || hadSomeDhall)
            [ "--help", "-h"
            , "--init"

            , "--dhall", "--dhall-diff", "--dhall-format", "--dhall-freeze"
            , "--dhall-hash", "--dhall-repl", "--dhall-resolve"
            ]
        <> munless (hadHelp || hadSomeDhall || not hadInit) ["--toolset="]
        <> munless
            ( List.or
                [ hadHelp, not hadDhall, hadDhallDiff, hadDhallFreeze
                , hadDhallFormat, hadDhallHash, hadDhallRepl, hadDhallResolve
                , hadInit
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
                    [hadDhall, hadDhallDiff, hadDhallFreeze, hadDhallResolve]
                , hadDhallFormat, hadDhallHash, hadDhallRepl, hadInit
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
--      <> munless
--          ( List.or
--              [ hadHelp, hadDhall, hadDhallDiff, hadDhallFreeze
--              , hadDhallFormat, hadDhallHash, hadDhallRepl
--              , not hadDhallResolve, hadInit
--              ]
--          )
--          ["--list=immediate", "--list=transitive"]

    matchingOptions = List.filter (pat `List.isPrefixOf`) possibleOptions

    -- TODO: If there is only one completion option and it is a directory we
    -- need to append "/" to it, or it will break the completion flow.
    bashCompleter :: String -> String -> IO [String]
    bashCompleter action prefix = fmap (prefix <>)
        <$> Options.runCompleter (Options.bashCompleter action)
            (List.drop (length prefix) pat)
