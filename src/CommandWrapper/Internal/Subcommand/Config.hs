{-# LANGUAGE DataKinds #-}
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

import Control.Applicative ((*>), (<*>), (<|>), many, optional, pure)
import Control.Monad (when)
import Data.Bool (Bool(False, True), (&&), (||), not, otherwise)
import qualified Data.Char as Char (isDigit, toLower)
import Data.Either (Either(Left, Right))
import Data.Eq ((==))
import Data.Foldable (asum, null)
import Data.Function (($), (.), const)
import Data.Functor (Functor, (<$>), (<&>), fmap)
import qualified Data.List as List
    ( any
    , dropWhile
    , elem
    , filter
    , isPrefixOf
    , or
    , take
    )
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Maybe (Maybe(Just), fromMaybe)
import Data.Monoid (Endo(Endo, appEndo), (<>), mconcat, mempty)
import Data.String (String, fromString)
import Data.Word (Word)
import GHC.Generics (Generic)
--import System.Exit (ExitCode(ExitFailure), exitWith)
import System.IO (IO{-, stderr-}, stdout)
import Text.Show (Show)

import qualified Data.CaseInsensitive as CI (mk)
import Data.Generics.Product.Fields (HasField')
import Data.Monoid.Endo.Fold (dualFoldEndo)
import Data.Output (HasOutput(Output), setOutput)
import Data.Text (Text)
import qualified Data.Text as Text (break, drop, null)
import Data.Text.Prettyprint.Doc ((<+>){-, pretty-})
import qualified Data.Text.Prettyprint.Doc as Pretty
    ( Doc
    , braces
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
    , ReadM
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
import qualified CommandWrapper.External as External (findSubcommands)
import CommandWrapper.Internal.Utils (runMain)
import CommandWrapper.Message
    ( Result
--  , errorMsg
    , out
    )
import qualified CommandWrapper.Options.Optparse as Options
    ( bashCompleter
    , internalSubcommandParse
--  , splitArguments
--  , splitArguments'
    )
import qualified CommandWrapper.Options.Shell as Options (Shell)
import CommandWrapper.Internal.Subcommand.Config.IsInput (IsInput, parseInput)
import qualified CommandWrapper.Internal.Subcommand.Config.Dhall as Dhall
    ( Bash(..)
    , BashMode(..)
    , Diff(..)
    , Exec(..)
    , Filter(..)
    , Format(..)
    , Freeze(..)
    , FreezePurpose(..)
    , HasInput
    , Hash(..)
    , Input(..)
    , Interpreter(..)
    , Lint(..)
    , Output(..)
    , Repl(..)
    , Resolve(..)
    , ResolveMode(..)
    , SemanticCacheMode(..)
    , ToText(..)
    , ToTextMode(..)
    , Variable(..)
    , addVariable
    , bash
    , defBash
    , defDiff
    , defExec
    , defFilter
    , defFormat
    , defFreeze
    , defHash
    , defInterpreter
    , defLint
    , defRepl
    , defResolve
    , defToText
    , diff
    , exec
    , filter
    , format
    , freeze
    , hash
    , interpreter
    , lint
    , repl
    , resolve
    , setAllowImports
    , setAlpha
    , setAnnotate
    , setFreezePurpose
    , setInput
    , setSemanticCacheMode
    , setShowType
    , toText
    )
import CommandWrapper.Internal.Subcommand.Config.Edit
    ( EditOptions(..)
    , WhatToEdit(..)
    , defEditOptions
    , edit
    )
import CommandWrapper.Internal.Subcommand.Config.Init
    ( InitOptions(..)
    , defInitOptions
    , init
    )
import CommandWrapper.Internal.Subcommand.Config.Menu
    ( MenuOptions(..)
    , defMenuOptions
    , menu
    )
import qualified CommandWrapper.Internal.Subcommand.Config.Menu as Menu
    ( Input(InputItems)
    , setInput
    )


data ConfigMode a
    = Init InitOptions a
    | ConfigLib a
    | Edit EditOptions a
    | Menu MenuOptions a
    | Dhall Dhall.Interpreter a
    | DhallDiff Dhall.Diff a
    | DhallFormat Dhall.Format a
    | DhallFreeze Dhall.Freeze a
    | DhallHash Dhall.Hash a
    | DhallLint Dhall.Lint a
    | DhallRepl Dhall.Repl a
    | DhallResolve Dhall.Resolve a
    | DhallExec Dhall.Exec a
    | DhallBash Dhall.Bash a
    | DhallText Dhall.ToText a
    | DhallFilter Dhall.Filter a
    | Help a
  deriving stock (Functor, Generic, Show)

config :: AppNames -> [String] -> Global.Config -> IO ()
config appNames options globalConfig =
    runMain (parseOptions appNames globalConfig options) defaults \case
        Init opts cfg ->
            init appNames cfg opts

        ConfigLib _ -> pure ()

        Edit opts cfg ->
            edit appNames cfg opts

        Menu opts cfg ->
            menu appNames cfg opts

        Dhall opts cfg ->
            Dhall.interpreter appNames cfg opts

        DhallDiff opts cfg ->
            Dhall.diff appNames cfg opts

        DhallFormat opts cfg ->
            Dhall.format appNames cfg opts

        DhallFreeze opts cfg ->
            Dhall.freeze appNames cfg opts

        DhallHash opts cfg ->
            Dhall.hash appNames cfg opts

        DhallLint opts cfg ->
            Dhall.lint appNames cfg opts

        DhallRepl opts cfg ->
            Dhall.repl appNames cfg opts

        DhallResolve opts cfg ->
            Dhall.resolve appNames cfg opts

        DhallExec opts cfg ->
            Dhall.exec appNames cfg opts

        DhallBash opts cfg ->
            Dhall.bash appNames cfg opts

        DhallText opts cfg ->
            Dhall.toText appNames cfg opts

        DhallFilter opts cfg ->
            Dhall.filter appNames cfg opts

        Help config'@Global.Config{colourOutput, verbosity} ->
            out verbosity colourOutput stdout
                (configSubcommandHelp appNames config')
  where
    defaults = Mainplate.applySimpleDefaults (Help globalConfig)

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
                <*> many letOption
                <*> many (cacheFlag <|> noCacheFlag)
                <*> optional outputOption
                <*> optional (expressionOption <|> inputOption Dhall.setInput)
            )

    , dhallReplFlag

    , dhallDiffFlag
        <*> Options.strArgument mempty
        <*> Options.strArgument mempty
        <*> ( dualFoldEndo
                <$> optional outputOption
            )

    , dhallHashFlag
        <*> ( dualFoldEndo
                <$> many (cacheFlag <|> noCacheFlag)
                <*> optional (expressionOption <|> inputOption Dhall.setInput)
                <*> optional outputOption
            )

    , dhallFreezeFlag
        <*> ( dualFoldEndo
                <$> many (remoteOnlyFlag <|> noRemoteOnlyFlag)
                <*> many (forSecurityFlag <|> forCachingFlag)
                <*> optional outputOption
                <*> optional (expressionOption <|> inputOption Dhall.setInput)
            )

    , dhallFormatFlag
        <*> ( dualFoldEndo
--              <$> many (checkFlag <|> noCheckFlag)
                <$> optional outputOption
                <*> optional (expressionOption <|> inputOption Dhall.setInput)
            )

    , dhallLintFlag
        <*> ( dualFoldEndo
                <$> optional outputOption
                <*> optional (expressionOption <|> inputOption Dhall.setInput)
            )

    , dhallResolveFlag
        <*> ( dualFoldEndo
                <$> many (cacheFlag <|> noCacheFlag)
                <*> optional outputOption
                <*> optional (expressionOption <|> inputOption Dhall.setInput)
                <*> optional listDependenciesOption
            )

    , dhallBashFlag
        <*> ( dualFoldEndo
                <$> many (allowImportsFlag <|> noAllowImportsFlag)
                <*> many (cacheFlag <|> noCacheFlag)
                <*> optional declareOption
                <*> optional (expressionOption <|> inputOption Dhall.setInput)
                <*> optional outputOption
            )

    , dhallExecFlag
        <*> (expressionOption <|> inputOption Dhall.setInput)
        <*> ( dualFoldEndo
                <$> optional
                    ( setInterpreter
                        <$> interpreterOption
                        <*> many interpreterArgumentOption
                    )
                <*> many scriptArgument
            )

    , dhallTextFlag
        <*> ( dualFoldEndo
                <$> many (allowImportsFlag <|> noAllowImportsFlag)
                <*> many (cacheFlag <|> noCacheFlag)
                <*> many listFlag
                <*> many
                    ( nulFlag \opts ->
                        (opts :: Dhall.ToText){Dhall.outputDelimiter = '\0'}
                    )
                <*> optional (expressionOption <|> inputOption Dhall.setInput)
                <*> optional outputOption
            )

    , dhallFilterFlag
        <*> Options.strArgument (Options.metavar "EXPRESSION")
        <*> ( dualFoldEndo
                <$> many (allowImportsFlag <|> noAllowImportsFlag)
                <*> many (alphaFlag <|> noAlphaFlag)
                <*> many (annotateFlag <|> noAnnotateFlag)
                <*> many (typeFlag <|> noTypeFlag)
                <*> many (cacheFlag <|> noCacheFlag)
                <*> many letOption
                <*> optional outputOption
                <*> optional (expressionOption <|> inputOption Dhall.setInput)
            )

    , editFlag
        <*> ( dualFoldEndo
            <$> optional
                ( fileArgument
                <|> (subcommandConfigFlag <*> subcommandArgument)
                )
            )

    , menuFlag
        <*> ( dualFoldEndo
            <$> many (nulFlag \opts -> opts{delimiter = '\0'})
            <*> optional
                ( inputOption Menu.setInput
                <|> (argumentsFlag *> menuArguments)
                )
            )

    , helpFlag

    , pure mempty
    ]
  where
    switchTo :: ConfigMode Global.Config -> Endo (ConfigMode Global.Config)
    switchTo = Endo . const

    switchToHelpMode :: Endo (ConfigMode Global.Config)
    switchToHelpMode = switchTo (Help globalConfig)

    switchToInitMode :: Endo (ConfigMode Global.Config)
    switchToInitMode = switchTo (Init (defInitOptions usedName) globalConfig)

    switchToEditMode :: Endo EditOptions -> Endo (ConfigMode Global.Config)
    switchToEditMode (Endo f) = switchTo (Edit (f defEditOptions) globalConfig)

    switchToMenuMode :: Endo MenuOptions -> Endo (ConfigMode Global.Config)
    switchToMenuMode (Endo f) = switchTo (Menu (f defMenuOptions) globalConfig)

    switchToDhallHashMode :: Endo Dhall.Hash -> Endo (ConfigMode Global.Config)
    switchToDhallHashMode f =
        switchTo (DhallHash (f `appEndo` Dhall.defHash) globalConfig)

    switchToDhallReplMode :: Endo (ConfigMode Global.Config)
    switchToDhallReplMode = switchTo (DhallRepl Dhall.defRepl globalConfig)

    switchToDhallMode
        :: Endo Dhall.Interpreter
        -> Endo (ConfigMode Global.Config)
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

    switchToDhallExecMode
        :: Endo Dhall.Input
        -> Endo Dhall.Exec
        -> Endo (ConfigMode Global.Config)
    switchToDhallExecMode f g = switchTo (DhallExec opts globalConfig)
      where
        opts = g `appEndo` Dhall.defExec (f `appEndo` Dhall.InputStdin)

    switchToDhallBashMode
        :: Endo Dhall.Bash
        -> Endo (ConfigMode Global.Config)
    switchToDhallBashMode f =
        switchTo (DhallBash (f `appEndo` Dhall.defBash) globalConfig)

    switchToDhallTextMode
        :: Endo Dhall.ToText
        -> Endo (ConfigMode Global.Config)
    switchToDhallTextMode f =
        switchTo (DhallText (f `appEndo` Dhall.defToText) globalConfig)

    switchToDhallFilterMode
        :: Text
        -> Endo Dhall.Filter
        -> Endo (ConfigMode Global.Config)
    switchToDhallFilterMode expr f =
        switchTo (DhallFilter (f `appEndo` Dhall.defFilter expr) globalConfig)

    initFlag :: Options.Parser (Endo (ConfigMode Global.Config))
    initFlag = Options.flag mempty switchToInitMode (Options.long "init")

    dhallFlag
        :: Options.Parser
            (Endo Dhall.Interpreter -> Endo (ConfigMode Global.Config))
    dhallFlag =
        Options.flag mempty switchToDhallMode (Options.long "dhall")

    alphaFlag :: HasField' "alpha" a Bool => Options.Parser (Endo a)
    alphaFlag = Options.flag' (setAlpha True) (Options.long "alpha")

    noAlphaFlag :: HasField' "alpha" a Bool => Options.Parser (Endo a)
    noAlphaFlag = Options.flag' (setAlpha False) (Options.long "no-alpha")

    setAlpha :: HasField' "alpha" a Bool => Bool -> Endo a
    setAlpha = Endo . Dhall.setAlpha

    annotateFlag :: HasField' "annotate" a Bool => Options.Parser (Endo a)
    annotateFlag = Options.flag' (setAnnotate True) (Options.long "annotate")

    noAnnotateFlag :: HasField' "annotate" a Bool => Options.Parser (Endo a)
    noAnnotateFlag =
        Options.flag' (setAnnotate False) (Options.long "no-annotate")

    setAnnotate :: HasField' "annotate" a Bool => Bool -> Endo a
    setAnnotate = Endo . Dhall.setAnnotate

    allowImportsFlag
        :: HasField' "allowImports" a Bool => Options.Parser (a -> a)
    allowImportsFlag =
        Options.flag' (Dhall.setAllowImports True)
            (Options.long "allow-imports")

    noAllowImportsFlag
        :: HasField' "allowImports" a Bool => Options.Parser (a -> a)
    noAllowImportsFlag =
        Options.flag' (Dhall.setAllowImports False)
        (Options.long "no-allow-imports")

    typeFlag :: HasField' "showType" a Bool => Options.Parser (Endo a)
    typeFlag = Options.flag' (setShowType True) (Options.long "type")

    noTypeFlag :: HasField' "showType" a Bool => Options.Parser (Endo a)
    noTypeFlag = Options.flag' (setShowType False) (Options.long "no-type")

    setShowType :: HasField' "showType" a Bool => Bool -> Endo a
    setShowType = Endo . Dhall.setShowType

    expressionOption
        :: Dhall.HasInput a
        => Options.Parser (Endo a)
    expressionOption =
        Endo . Dhall.setInput . Dhall.InputExpression
            <$> Options.strOption
                (Options.long "expression" <> Options.metavar "EXPRESSION")

    inputOption :: IsInput a => (a -> b -> b) -> Options.Parser (Endo b)
    inputOption f =
        Endo . f
            <$> Options.option (Options.eitherReader parseInput)
                    (Options.long "input" <> Options.short 'i')

    outputOption
        :: (Output a ~ Dhall.Output, HasOutput a)
        => Options.Parser (Endo a)
    outputOption = Endo . setOutput <$> Options.outputOption

    letOption
        :: forall a
        .  HasField' "variables" a [Dhall.Variable]
        => Options.Parser (Endo a)
    letOption =
        Options.option parseLet
            (Options.long "let" <> Options.metavar "NAME=EXPRESSION")
      where
        parseLet :: Options.ReadM (Endo a)
        parseLet = Options.eitherReader \definition -> do
            let (variable, expr) =
                    Text.break (== '=') (fromString definition)

            when (Text.null variable || Text.null expr || expr == "=")
                (Left "Neither NAME nor EXPRESSION can be empty.")

            pure . Endo $ Dhall.addVariable Dhall.Variable
                { variable
                , value = Text.drop 1 expr  -- "=EXPRESSION" --> "EXPRESSION"
                }

    cacheFlag
        :: HasField' "semanticCache" a Dhall.SemanticCacheMode
        => Options.Parser (a -> a)
    cacheFlag =
        Options.flag' (Dhall.setSemanticCacheMode Dhall.UseSemanticCache)
            (Options.long "cache")

    noCacheFlag
        :: HasField' "semanticCache" a Dhall.SemanticCacheMode
        => Options.Parser (a -> a)
    noCacheFlag =
        Options.flag' (Dhall.setSemanticCacheMode Dhall.IgnoreSemanticCache)
            (Options.long "no-cache")

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

    forCachingFlag :: Options.Parser (Dhall.Freeze -> Dhall.Freeze)
    forCachingFlag =
        Options.flag' (Dhall.setFreezePurpose Dhall.FreezeForCaching)
            (Options.long "for-caching")

    forSecurityFlag :: Options.Parser (Dhall.Freeze -> Dhall.Freeze)
    forSecurityFlag =
        Options.flag' (Dhall.setFreezePurpose Dhall.FreezeForSecurity)
            (Options.long "for-security")

    dhallHashFlag
        :: Options.Parser (Endo Dhall.Hash -> Endo (ConfigMode Global.Config))
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
                (r :: Dhall.Resolve)
                    { Dhall.mode = Dhall.ListImmediateDependencies
                    }

            "transitive" -> Right $ Endo \r ->
                (r :: Dhall.Resolve)
                    { Dhall.mode = Dhall.ListTransitiveDependencies
                    }

            _ -> Left "Expected 'immediate' or 'transitive'"

    dhallExecFlag
        :: Options.Parser
            ( Endo Dhall.Input
            -> Endo Dhall.Exec
            -> Endo (ConfigMode Global.Config)
            )
    dhallExecFlag =
        Options.flag mempty switchToDhallExecMode (Options.long "dhall-exec")

    interpreterOption :: Options.Parser Text
    interpreterOption = Options.strOption
        (Options.long "interpreter" <> Options.metavar "COMMAND")

    interpreterArgumentOption :: Options.Parser Text
    interpreterArgumentOption = Options.strOption
        (Options.long "interpreter-argument" <> Options.metavar "ARGUMENT")

    setInterpreter :: Text -> [Text] -> Endo Dhall.Exec
    setInterpreter cmd args =
        Endo \opts -> opts
            { Dhall.interpret = Just (cmd :| args)
            }

    scriptArgument :: Options.Parser (Endo Dhall.Exec)
    scriptArgument =
        Options.strArgument (Options.metavar "ARGUMENT") <&> \argument ->
            Endo \opts@Dhall.Exec{arguments} -> opts
                { Dhall.arguments = arguments <> [argument]
                }

    dhallBashFlag
        :: Options.Parser
            (Endo Dhall.Bash -> Endo (ConfigMode Global.Config))
    dhallBashFlag =
        Options.flag mempty switchToDhallBashMode (Options.long "dhall-bash")

    declareOption :: Options.Parser (Endo Dhall.Bash)
    declareOption =
        Options.strOption (Options.long "declare" <> Options.metavar "NAME")
        <&> \name ->
            Endo \opts ->
                (opts :: Dhall.Bash){Dhall.mode = Dhall.BashStatementMode name}

    dhallTextFlag
        :: Options.Parser (Endo Dhall.ToText -> Endo (ConfigMode Global.Config))
    dhallTextFlag =
        Options.flag mempty switchToDhallTextMode (Options.long "dhall-text")

    listFlag :: Options.Parser (Endo Dhall.ToText)
    listFlag = Options.flag' setListMode (Options.long "list")
      where
        setListMode = Endo \opts ->
            (opts :: Dhall.ToText){Dhall.mode = Dhall.ListTextMode}

    dhallFilterFlag
        :: Options.Parser
            (Text -> Endo Dhall.Filter -> Endo (ConfigMode Global.Config))
    dhallFilterFlag =
        Options.flag mempty switchToDhallFilterMode
            (Options.long "dhall-filter")

    toolsetOption :: Options.Parser (Endo (ConfigMode Global.Config))
    toolsetOption =
        Options.strOption (Options.long "toolset" <> Options.metavar "NAME")
            <&> \toolsetName -> Endo \case
                    Init opts cfg ->
                        Init (opts :: InitOptions){toolsetName} cfg
                    mode ->
                        mode

    editFlag
        :: Options.Parser (Endo EditOptions -> Endo (ConfigMode Global.Config))
    editFlag = Options.flag mempty switchToEditMode $ mconcat
        [ Options.long "edit"
        , Options.short 'e'
        ]

    menuFlag
        :: Options.Parser (Endo MenuOptions -> Endo (ConfigMode Global.Config))
    menuFlag = Options.flag mempty switchToMenuMode (Options.long "menu")

    fileArgument :: Options.Parser (Endo EditOptions)
    fileArgument =
        Options.strArgument (Options.metavar "FILE") <&> \argument ->
            Endo \opts@EditOptions{} -> opts
                { what = Just (EditFile argument)
                }

    subcommandConfigFlag :: Options.Parser (String -> Endo EditOptions)
    subcommandConfigFlag =
        Options.flag' setSubcommand (Options.long "subcommand-config")
      where
        setSubcommand argument =
            Endo \opts@EditOptions{} -> opts
                { what = Just (EditSubcommandConfig argument)
                }

    subcommandArgument :: Options.Parser String
    subcommandArgument = Options.strArgument (Options.metavar "SUBCOMMAND")

    argumentsFlag :: Options.Parser ()
    argumentsFlag = Options.flag' () (Options.long "arguments")

    menuArguments :: Options.Parser (Endo MenuOptions)
    menuArguments =
        Endo . Menu.setInput . Menu.InputItems
            <$> many (Options.strArgument (Options.metavar "STRING"))

    nulFlag :: (a -> a) -> Options.Parser (Endo a)
    nulFlag f = Options.flag' (Endo f) $ mconcat
        [ Options.long "null"   -- Used e.g. by `xargs`
        , Options.long "nul"    -- ASCII name for the character.
        , Options.short '0'     -- This one is used by e.g. `xargs`
        , Options.short 'z'     -- Git likes this one
        ]

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
            <+> Pretty.brackets (longOption "[no-]allow-imports")
            <+> Pretty.brackets (longOption "[no-]alpha")
            <+> Pretty.brackets (longOption "[no-]annotate")
            <+> Pretty.brackets (longOption "[no-]cache")
            <+> Pretty.brackets (longOption "[no-]type")
            <+> Pretty.brackets
                ( longOptionWithArgument "let" "NAME=EXPRESSION"
                )
            <+> Pretty.brackets
                ( longOptionWithArgument "expression" "EXPRESSION"
                <> "|" <> longOptionWithArgument "input" "FILE"
                )
            <+> Pretty.brackets (longOptionWithArgument "output" "FILE")

        , "config"
            <+> longOption "dhall-filter"
            <+> Pretty.brackets (longOption "[no-]allow-imports")
            <+> Pretty.brackets (longOption "[no-]alpha")
            <+> Pretty.brackets (longOption "[no-]annotate")
            <+> Pretty.brackets (longOption "[no-]cache")
            <+> Pretty.brackets (longOption "[no-]type")
            <+> Pretty.brackets
                ( longOptionWithArgument "let" "NAME=EXPRESSION"
                )
            <+> Pretty.brackets
                ( longOptionWithArgument "expression" "EXPRESSION"
                <> "|" <> longOptionWithArgument "input" "FILE"
                )
            <+> Pretty.brackets (longOptionWithArgument "output" "FILE")
            <+> metavar "EXPRESSION"

        , "config"
            <+> longOption "dhall-format"
--          <+> Pretty.brackets (longOption "[no-]check")
            <+> Pretty.brackets
                ( longOptionWithArgument "expression" "EXPRESSION"
                <> "|" <> longOptionWithArgument "input" "FILE"
                )
            <+> Pretty.brackets (longOptionWithArgument "output" "FILE")

        , "config"
            <+> longOption "dhall-lint"
            <+> Pretty.brackets
                ( longOptionWithArgument "expression" "EXPRESSION"
                <> "|" <> longOptionWithArgument "input" "FILE"
                )
            <+> Pretty.brackets (longOptionWithArgument "output" "FILE")

        , "config"
            <+> longOption "dhall-resolve"
            <+> Pretty.brackets (longOption "[no-]cache")
            <+> Pretty.brackets (longOptionWithArgument "list-imports" "KIND")
            <+> Pretty.brackets
                ( longOptionWithArgument "expression" "EXPRESSION"
                <> "|" <> longOptionWithArgument "input" "FILE"
                )
            <+> Pretty.brackets (longOptionWithArgument "output" "FILE")

        , "config"
            <+> longOption "dhall-freeze"
            <+> Pretty.brackets (longOption "[no-]remote-only")
            <+> Pretty.brackets
                ( longOption "for-security"
                <> "|" <> longOption "for-caching"
                )
            <+> Pretty.brackets
                ( longOptionWithArgument "expression" "EXPRESSION"
                <> "|" <> longOptionWithArgument "input" "FILE"
                )
            <+> Pretty.brackets (longOptionWithArgument "output" "FILE")

        , "config"
            <+> longOption "dhall-hash"
            <+> Pretty.brackets (longOption "[no-]cache")
            <+> Pretty.brackets
                ( longOptionWithArgument "expression" "EXPRESSION"
                <> "|" <> longOptionWithArgument "input" "FILE"
                )
            <+> Pretty.brackets (longOptionWithArgument "output" "FILE")

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
            <+> longOption "dhall-bash"
            <+> Pretty.brackets (longOption "[no-]allow-imports")
            <+> Pretty.brackets (longOption "[no-]cache")
            <+> Pretty.brackets (longOptionWithArgument "declare" "NAME")
            <+> Pretty.brackets
                ( longOptionWithArgument "expression" "EXPRESSION"
                <> "|" <> longOptionWithArgument "input" "FILE"
                )
            <+> Pretty.brackets (longOptionWithArgument "output" "FILE")

        , "config"
            <+> longOption "dhall-text"
            <+> Pretty.brackets (longOption "[no-]allow-imports")
            <+> Pretty.brackets (longOption "[no-]cache")
            <+> Pretty.brackets
                ( longOption "list"
                <+> Pretty.brackets (longOption "nul[l]")
                )
            <+> Pretty.brackets
                ( longOptionWithArgument "expression" "EXPRESSION"
                <> "|" <> longOptionWithArgument "input" "FILE"
                )
            <+> Pretty.brackets (longOptionWithArgument "output" "FILE")

        , "config"
            <+> longOption "dhall-exec"
            <+> Pretty.braces
                ( longOptionWithArgument "expression" "EXPRESSION"
                <> "|" <> longOptionWithArgument "input" "FILE"
                )
            <+> Pretty.brackets
                ( longOptionWithArgument "interpreter" "COMMAND"
                <+> Pretty.brackets
                    ( longOptionWithArgument "interpreter-argument" "ARGUMENT"
                    <+> "..."
                    )
                )
            <+> Pretty.brackets (metavar "ARGUMENT" <+> "...")

        , "config"
            <+> longOption "edit"
            <+> Pretty.brackets
                ( metavar "FILE"
                <> "|" <> longOption "subcommand-config"
                    <+> metavar "SUBCOMMAND"
                )

        , "config"
            <+> longOption "menu"
            <+> Pretty.brackets (longOption "nul[l]")
            <+> Pretty.brackets
                ( longOptionWithArgument "input" "FILE"
                <> "|" <> longOption "arguments"
                    <+> Pretty.brackets (metavar "STRING" <+> "...")
                )

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

        , optionDescription
            [ "--expression=EXPRESSION"
            , "--expression EXPRESSION"
            ]
            [ "Use", metavar "EXPRESSION", Pretty.reflow "as an input instead\
                \ of reading it from standard input or from a"
            , metavar "FILE" <> "."
            ]

        , optionDescription ["--input=FILE", "--input FILE", "-i FILE"]
            [ Pretty.reflow "Read input from", metavar "FILE"
            , Pretty.reflow "instead of standard input."
            ]

        , optionDescription ["--output=FILE", "--output FILE", "-o FILE"]
            [ Pretty.reflow "Write output into", metavar "FILE"
            , Pretty.reflow "instead of standard output."
            ]

        , optionDescription ["--[no-]cache"]
            [ Pretty.reflow
                "Specifies if caching should be used when resolving imports\
                \ protected by hash.  By default cache is used."
            ]

        , optionDescription ["--let=NAME=EXPRESSION"]
            [ Pretty.reflow "Declare variable", metavar "NAME"
            , Pretty.reflow "with it's value set to"
            , metavar "EXPRESSION" <> ","
            , Pretty.reflow "as if it was part of the input."
            ]

        , optionDescription ["--dhall-filter"]
            [ Pretty.reflow "Puts Dhall input expression into the scope of"
            , metavar "EXPRESSION", "as", value "input : Input" <> "."
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

        , optionDescription ["--for-security", "--for-caching"]
            [ Pretty.reflow
                "Specifies if integrity checks should be added for the purpose\
                \ of security or caching.  If for caching then alternative\
                \ import without integrity hash is added.  By default we\
                \ assume that freeze is for security purposes."
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

        , optionDescription ["--dhall-bash"]
            [ Pretty.reflow "Compile Dhall expression into Bash expression or\
                \ statement."
            ]

        , optionDescription ["--declare=NAME"]
            [ Pretty.reflow "Compile Dhall expression into a declaration\
                \ statement, which declares variable"
            , metavar "NAME" <> "."
            ]

        , optionDescription ["--dhall-text"]
            [ Pretty.reflow "Render Dhall expression as a text. This allows us\
                \ to use Dhall as a text templating language."
            ]

        , optionDescription ["--list"]
            [ Pretty.reflow "Render Dhall expression of type"
            , value "List Text"
            , Pretty.reflow "as lines of text."
            ]

        , optionDescription ["--dhall-exec"]
            [ Pretty.reflow "Render Dhall expression as Text and execute the\
                \ result. See also"
            , longOptionWithArgument "interpreter" "COMMAND" <> "."
            ]

        , optionDescription ["--interpreter=COMMAND"]
            [ Pretty.reflow "Run rendered Dhall expression as a script using"
            , metavar "COMMAND", Pretty.reflow "as an interpreter. See also"
            , longOptionWithArgument "interpreter-argument" "ARGUMENT" <> "."
            ]

        , optionDescription ["--interpreter-argument=ARGUMENT"]
            [ "Pass", metavar "ARGUMENT", "to", "interpreter"
            , metavar "COMMAND" <> "."
            ]

        , optionDescription ["--init"]
            [ Pretty.reflow "Initialise configuration of a toolset."
            ]

        , optionDescription ["--toolset=NAME"]
            [ Pretty.reflow "When specified allong with", longOption "init"
            , Pretty.reflow "then configuration for toolset", metavar "NAME"
            , Pretty.reflow "is initialised."
            ]

        , optionDescription ["--edit", "-e"]
            [ Pretty.reflow "Start editor to edit", metavar "FILE", "or"
            , metavar "SUBCOMMAND", "when", longOption "subcommand-config"
            , Pretty.reflow "is passed."
            ]

        , optionDescription ["--subcommand-config"]
            [ Pretty.reflow "Open subcommand config when invoked with"
            , longOption "edit" <> "."
            ]

        , optionDescription ["--menu"]
            [ Pretty.reflow "Display selection menu. Selected value is printed\
                \ to standard output."
            ]

        , optionDescription ["--arguments [STRING ...]"]
            [ Pretty.reflow "Use arguments", "(" <> metavar "STRING" <> "s)"
            , Pretty.reflow "as input."
            ]

        , optionDescription ["--nul[l], -0, -z"]
            [ "Use", value "NUL", "('" <> value "\\0" <> "')"
            , Pretty.reflow "character as a separator."
            ]

        , optionDescription ["--help", "-h"]
            [ Pretty.reflow "Print this help and exit. Same as"
            , Pretty.squotes (toolsetCommand usedName "help config") <> "."
            ]

        , optionDescription ["EPRESSION"]
            [ "Dhall", metavar "EXPRESSION" <> "."
            ]

        , optionDescription ["ARGUMENT"]
            [ Pretty.reflow "Command line argument passed to executed script in"
            , longOption "dhall-exec", "mode."
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
configSubcommandCompleter appNames cfg _shell index words
  | Just "-o" <- lastMay wordsBeforePattern =
        bashCompleter "file" ""

  | Just "--output" <- lastMay wordsBeforePattern =
        bashCompleter "file" ""

  | Just "-i" <- lastMay wordsBeforePattern =
        bashCompleter "file" ""

  | Just "--input" <- lastMay wordsBeforePattern =
        bashCompleter "file" ""

  | Just "--interpreter" <- lastMay wordsBeforePattern =
        bashCompleter "command" ""

  -- TODO: This may be Bash-scpecific.  We need to investigate other shells.
  | Just w <- lastMay wordsBeforePattern, isBashRedirection w =
        bashCompleter "file" ""

  | Just "--edit" <- lastMay wordsBeforePattern =
        bashCompleter "file" ""

  | Just "-e" <- lastMay wordsBeforePattern =
        bashCompleter "file" ""

  | Just "--subcommand-config" <- lastMay wordsBeforePattern =
        List.filter (fmap Char.toLower pat `List.isPrefixOf`)
            <$> External.findSubcommands appNames cfg

  | "--output=" `List.isPrefixOf` pat =
        bashCompleter "file" "--output="

  | "--input=" `List.isPrefixOf` pat =
        bashCompleter "file" "--input="

  | "--interpreter=" `List.isPrefixOf` pat =
        bashCompleter "command" "--interpreter="

{-
  | "--let=" `List.isPrefixOf` pat =
        TODO: Figure out if it contains another '=' sign to do completion on
        file.
        bashCompleter "file" ("--let=" <> variableName <> "=")
-}

  | null pat =
        pure possibleOptions

  | otherwise =
        pure matchingOptions
  where
    wordsBeforePattern = List.take (fromIntegral index) words
    pat = fromMaybe "" $ atMay words (fromIntegral index)

    hadEdit =
        ("--edit" `List.elem` wordsBeforePattern)
        || ("-e" `List.elem` wordsBeforePattern)

    hadHelp =
        ("--help" `List.elem` wordsBeforePattern)
        || ("-h" `List.elem` wordsBeforePattern)

    hadInit = "--init" `List.elem` wordsBeforePattern

    hadMenu = "--menu" `List.elem` wordsBeforePattern

    hadDhall = "--dhall" `List.elem` wordsBeforePattern
    hadDhallBash = "--dhall-bash" `List.elem` wordsBeforePattern
    hadDhallDiff = "--dhall-diff" `List.elem` wordsBeforePattern
    hadDhallExec = "--dhall-exec" `List.elem` wordsBeforePattern
    hadDhallFilter = "--dhall-filter" `List.elem` wordsBeforePattern
    hadDhallFormat = "--dhall-format" `List.elem` wordsBeforePattern
    hadDhallFreeze = "--dhall-freeze" `List.elem` wordsBeforePattern
    hadDhallHash = "--dhall-hash" `List.elem` wordsBeforePattern
    hadDhallLint = "--dhall-lint" `List.elem` wordsBeforePattern
    hadDhallRepl = "--dhall-repl" `List.elem` wordsBeforePattern
    hadDhallResolve = "--dhall-resolve" `List.elem` wordsBeforePattern
    hadDhallText = "--dhall-text" `List.elem` wordsBeforePattern

    hadSomeDhall = List.or
        [ hadDhall
        , hadDhallBash
        , hadDhallDiff
        , hadDhallExec
        , hadDhallFilter
        , hadDhallFormat
        , hadDhallFreeze
        , hadDhallHash
        , hadDhallLint
        , hadDhallRepl
        , hadDhallResolve
        , hadDhallText
        ]

    hadOutput = List.or
        [ "-o" `List.elem` wordsBeforePattern
        , "--output" `List.elem` wordsBeforePattern
        , List.any ("--output=" `List.isPrefixOf`) wordsBeforePattern
        ]

    hadInput = List.or
        [ "-i" `List.elem` wordsBeforePattern
        , "--input" `List.elem` wordsBeforePattern
        , List.any ("--input=" `List.isPrefixOf`) wordsBeforePattern
        ]

    hadExpression = List.or
        [ "--expression" `List.elem` wordsBeforePattern
        , List.any ("--expression=" `List.isPrefixOf`) wordsBeforePattern
        ]

    hadInterpreter = List.or
        [ "--interpreter" `List.elem` wordsBeforePattern
        , List.any ("--interpreter=" `List.isPrefixOf`) wordsBeforePattern
        ]

    hadDeclare = List.or
        [ "--declare" `List.elem` wordsBeforePattern
        , List.any ("--declare=" `List.isPrefixOf`) wordsBeforePattern
        ]

    hadToolset = List.or
        [ "--toolset" `List.elem` wordsBeforePattern
        , List.any ("--toolset=" `List.isPrefixOf`) wordsBeforePattern
        ]

    hadSubcommandConfig =
        "--subcommand-config" `List.elem` wordsBeforePattern

    hadArguments =
        "--arguments" `List.elem` wordsBeforePattern

    mwhen p x = if p then x else mempty
    munless p = mwhen (not p)

    possibleOptions =
        munless (List.or [hadEdit, hadHelp, hadInit, hadMenu, hadSomeDhall])
            [ "--dhall"
            , "--dhall-bash"
            , "--dhall-diff"
            , "--dhall-exec"
            , "--dhall-filter"
            , "--dhall-format"
            , "--dhall-freeze"
            , "--dhall-hash"
            , "--dhall-lint"
            , "--dhall-repl"
            , "--dhall-resolve"
            , "--dhall-text"
            , "--edit", "-e"
            , "--help", "-h"
            , "--init"
            , "--menu"
            ]
        <> mwhen (hadInit && not hadToolset) ["--toolset="]
        <> mwhen (hadDhall || hadDhallFilter)
            [ "--alpha", "--no-alpha"
            , "--annotate", "--no-annotate"
            , "--type", "--no-type"
            ]
        <> munless
            ( List.or
                [ hadDhallDiff
                , hadDhallExec
                , hadDhallFormat
                , hadDhallFreeze
                , hadDhallHash
                , hadDhallLint
                , hadDhallRepl
                , hadDhallResolve
                , hadEdit
                , hadHelp
                , hadInit
                , hadMenu

                , not $ List.or
                    [ hadDhall
                    , hadDhallBash
                    , hadDhallFilter
                    , hadDhallText
                    ]
                ]
            )
            [ "--allow-imports", "--no-allow-imports"
            ]
        <> munless
            ( List.or
                [ hadDhallExec
                , hadDhallRepl
                , hadHelp
                , hadInit
                , hadMenu

                -- Output options can be specified only once.
                , hadOutput

                , not $ List.or
                    [ hadDhall
                    , hadDhallBash
                    , hadDhallDiff
                    , hadDhallFilter
                    , hadDhallFormat
                    , hadDhallFreeze
                    , hadDhallHash
                    , hadDhallLint
                    , hadDhallResolve
                    , hadDhallText
                    ]
                ]
            )
            ["-o", "--output="]
        <> munless
            ( List.or
                [ hadDhallDiff
                , hadDhallRepl
                , hadHelp
                , hadInit

                -- Input options can be specified only once.
                , hadInput

                -- '--input' and '--expression' are mutually exclusive.
                , hadExpression

                -- Once '--arguments' was passed we can't pass '--input' any
                -- more.
                , hadMenu && hadArguments

                , not $ List.or
                    [ hadDhall
                    , hadDhallBash
                    , hadDhallDiff
                    , hadDhallExec
                    , hadDhallFilter
                    , hadDhallFormat
                    , hadDhallFreeze
                    , hadDhallHash
                    , hadDhallLint
                    , hadDhallResolve
                    , hadDhallText
                    , hadMenu
                    ]
                ]
            )
            ["-i", "--input="]
        <> munless
            ( List.or
                [ hadDhallDiff
                , hadDhallRepl
                , hadHelp
                , hadInit
                , hadMenu

                -- Expression options can be specified only once.
                , hadExpression

                -- '--input' and '--expression' are mutually exclusive.
                , hadInput

                , not $ List.or
                    [ hadDhall
                    , hadDhallBash
                    , hadDhallExec
                    , hadDhallFilter
                    , hadDhallFormat
                    , hadDhallFreeze
                    , hadDhallHash
                    , hadDhallLint
                    , hadDhallResolve
                    , hadDhallText
                    ]
                ]
            )
            ["--expression="]
        <> munless
            ( List.or
                [ hadDhall
                , hadDhallBash
                , hadDhallDiff
                , hadDhallExec
                , hadDhallFilter
                , hadDhallFormat
                , hadDhallHash
                , hadDhallLint
                , hadDhallRepl
                , hadDhallResolve
                , hadDhallText
                , hadHelp
                , hadInit
                , hadMenu

                , not hadDhallFreeze
                ]
            )
            ["--remote-only", "--no-remote-only"]
--      <> munless
--          ( List.or
--              [ hadDhall
--              , hadDhallBash
--              , hadDhallDiff
--              , hadDhallExec
--              , hadDhallFilter
--              , hadDhallFreeze
--              , hadDhallHash
--              , hadDhallLint
--              , hadDhallRepl
--              , hadDhallResolve
--              , hadDhallText
--              , hadHelp
--              , hadInit
--              , hadMenu
--
--              , not hadDhallFormat
--              ]
--          )
--          ["--check", "--no-check"]
        <> munless
            ( List.or
                [ hadDhall
                , hadDhallBash
                , hadDhallDiff
                , hadDhallExec
                , hadDhallFilter
                , hadDhallFormat
                , hadDhallFreeze
                , hadDhallHash
                , hadDhallLint
                , hadDhallRepl
                , hadDhallText
                , hadHelp
                , hadInit
                , hadMenu

                , not hadDhallResolve
                ]
            )
            ["--list-imports=immediate", "--list-imports=transitive"]
        <> munless
            ( List.or
                [ hadDhall
                , hadDhallBash
                , hadDhallDiff
                , hadDhallFilter
                , hadDhallFormat
                , hadDhallFreeze
                , hadDhallHash
                , hadDhallLint
                , hadDhallRepl
                , hadDhallResolve
                , hadDhallText
                , hadHelp
                , hadInit
                , hadMenu

                -- This option can be specified only once.
                , hadInterpreter

                , not hadDhallExec
                ]
            )
            ["--interpreter="]
        <> munless
            ( List.or
                [ hadDhall
                , hadDhallBash
                , hadDhallDiff
                , hadDhallFilter
                , hadDhallFormat
                , hadDhallFreeze
                , hadDhallHash
                , hadDhallLint
                , hadDhallRepl
                , hadDhallResolve
                , hadDhallText
                , hadHelp
                , hadInit
                , hadMenu

                , not hadDhallExec
                ]
            )
            ["--interpreter-argument="]
        <> munless
            ( List.or
                [ hadDhall
                , hadDhallDiff
                , hadDhallExec
                , hadDhallFilter
                , hadDhallFormat
                , hadDhallFreeze
                , hadDhallHash
                , hadDhallLint
                , hadDhallRepl
                , hadDhallResolve
                , hadDhallText
                , hadHelp
                , hadInit
                , hadMenu

                -- This option can be specified only once.
                , hadDeclare

                , not hadDhallBash
                ]
            )
            ["--declare="]
        <> munless
            ( List.or
                [ hadDhall
                , hadDhallBash
                , hadDhallDiff
                , hadDhallExec
                , hadDhallFilter
                , hadDhallFormat
                , hadDhallFreeze
                , hadDhallHash
                , hadDhallLint
                , hadDhallRepl
                , hadDhallResolve
                , hadHelp
                , hadInit
                , hadMenu

                , not hadDhallText
                ]
            )
            ["--list"]
        <> munless
            ( List.or
                [ hadDhallBash
                , hadDhallDiff
                , hadDhallExec
                , hadDhallFormat
                , hadDhallFreeze
                , hadDhallHash
                , hadDhallLint
                , hadDhallRepl
                , hadDhallResolve
                , hadDhallText
                , hadHelp
                , hadInit
                , hadMenu

                , not $ List.or
                    [ hadDhall
                    , hadDhallFilter
                    ]
                ]
            )
            ["--let="]
        <> munless
            ( List.or
                [ hadDhallDiff
                , hadDhallExec
                , hadDhallFormat
                , hadDhallFreeze
                , hadDhallLint
                , hadDhallRepl
                , hadHelp
                , hadInit
                , hadMenu

                , not $ List.or
                    [ hadDhall
                    , hadDhallBash
                    , hadDhallFilter
                    , hadDhallHash
                    , hadDhallResolve
                    , hadDhallText
                    ]
                ]
            )
            ["--cache", "--no-cache"]
        <> munless
            ( List.or
                [ hadDhall
                , hadDhallBash
                , hadDhallDiff
                , hadDhallExec
                , hadDhallFilter
                , hadDhallFormat
                , hadDhallHash
                , hadDhallLint
                , hadDhallRepl
                , hadDhallResolve
                , hadDhallText
                , hadHelp
                , hadInit
                , hadMenu

                , not hadDhallFreeze
                ]
            )
            ["--for-security", "--for-caching"]
        <> mwhen (hadEdit && not hadSubcommandConfig) ["--subcommand-config"]
        <> mwhen (hadMenu && not hadArguments) ["--arguments"]
        <> mwhen
            ( List.or
                [ hadMenu && not hadArguments
                , hadDhallText
                ]
            )
            ["--nul", "-0"]

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
        , "<<<"
        ]

    bashCompleter a p = Options.bashCompleter a p pat
