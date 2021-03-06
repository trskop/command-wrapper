{-# LANGUAGE DataKinds #-}
-- |
-- Module:      $Header$
-- Description: Implementation of internal command named config
-- Copyright:   (c) 2018-2020 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Implementation of internal command named @config@.
module CommandWrapper.Toolset.InternalSubcommand.Config
    ( config
    , configSubcommandDescription
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
import Data.Foldable (asum, length, null)
import Data.Function (($), (.), const)
import Data.Functor (Functor, (<$>), (<&>), fmap)
import qualified Data.List as List
    ( any
    , drop
    , dropWhile
    , elem
    , filter
    , isPrefixOf
    , or
    , take
    )
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Maybe (Maybe(Just, Nothing), fromMaybe)
import Data.Monoid (Endo(Endo, appEndo), (<>), mconcat, mempty)
import Data.String (String, fromString)
import Data.Word (Word)
import GHC.Generics (Generic)
--import System.Exit (ExitCode(ExitFailure), exitWith)
import System.IO (FilePath, IO{-, stderr-}, stdout)
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
import qualified Dhall (embed, inject)
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

import CommandWrapper.Core.Completion.EnvironmentVariables
    ( EnvironmentVariablesOptions(word, prefix)
    , defEnvironmentVariablesOptions
    , environmentVariablesCompleter
    )
import CommandWrapper.Core.Completion.FileSystem
    ( FileSystemOptions
        ( appendSlashToSingleDirectoryResult
        , expandTilde
        , prefix
        , word
        )
    , defFileSystemOptions
    , fileSystemCompleter
    )
import CommandWrapper.Core.Config.Shell (Shell(Bash, Fish, Zsh))
import CommandWrapper.Core.Environment (AppNames(AppNames, usedName))
import CommandWrapper.Core.Help.Pretty
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
import CommandWrapper.Core.Message
    ( Result
--  , errorMsg
    , out
    )
import qualified CommandWrapper.Toolset.Config.Global as Global
    ( Config(Config, colourOutput, verbosity)
    )
import qualified CommandWrapper.Toolset.ExternalSubcommand as External
    ( findSubcommands
    )
import CommandWrapper.Toolset.InternalSubcommand.Config.IsInput
    ( IsInput, parseInput
    )
import qualified CommandWrapper.Toolset.InternalSubcommand.Config.Dhall as Dhall
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
    , OutputOrCheck
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
    , hPutExpr
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
    , setOnlySecureImpors
    , setSemanticCacheMode
    , setShowType
    , toText
    )
import CommandWrapper.Toolset.InternalSubcommand.Config.Edit
    ( EditOptions(..)
    , WhatToEdit(..)
    , defEditOptions
    , edit
    )
import CommandWrapper.Toolset.InternalSubcommand.Config.Init
    ( InitOptions(..)
    , defInitOptions
    , init
    )
import CommandWrapper.Toolset.InternalSubcommand.Config.Menu
    ( MenuOptions(..)
    , defMenuOptions
    , menu
    )
import qualified CommandWrapper.Toolset.InternalSubcommand.Config.Menu as Menu
    ( Input(InputItems)
    , setInput
    )
import CommandWrapper.Toolset.InternalSubcommand.Utils (runMain)
import qualified CommandWrapper.Toolset.InternalSubcommand.Utils as Options
    ( dhallFlag
    , helpFlag
    , toolsetOption
    )
import qualified CommandWrapper.Toolset.Options.Optparse as Options
    ( bashCompleter
    , internalSubcommandParse
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
    | Get GetOptions a
    | Help a
  deriving stock (Functor, Generic, Show)

config :: AppNames -> [String] -> Global.Config -> IO ()
config appNames options globalConfig =
    runMain (parseOptions appNames globalConfig options) defaults \case
        Init opts cfg ->
            init appNames cfg opts

        ConfigLib _ -> pure ()

        Get GetOptions{} cfg ->
            Dhall.hPutExpr cfg stdout (Dhall.embed Dhall.inject globalConfig)

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

data GetOptions = GetOptions
--  { input :: Input
--  , output :: Output
--  }
  deriving stock (Generic, Show)

defGetOptions :: GetOptions
defGetOptions = GetOptions{}

parseOptions
    :: AppNames
    -> Global.Config
    -> [String]
    -> IO (Endo (ConfigMode Global.Config))
parseOptions appNames@AppNames{usedName} globalConfig options = execParser
    [ initFlag
        <*> ( dualFoldEndo
                <$> optional toolsetOption
                <*> many (configDirOption \d opts -> opts{configDir = Just d})
                <*> many (binDirOption \d opts -> opts{binDir = Just d})
                <*> many (libexecDirOption \d opts -> opts{libexecDir = Just d})
                <*> many (manDirOption \d opts -> opts{manDir = Just d})
            )

    , Options.dhallFlag switchToDhallMode
        <*> ( dualFoldEndo
                <$> many (allowImportsFlag <|> noAllowImportsFlag)
                <*> many
                    ( secureRemoteImportsImportsFlag
                    <|> noSecureRemoteImportsImportsFlag
                    )
                <*> many (alphaFlag <|> noAlphaFlag)
                <*> many (annotateFlag <|> noAnnotateFlag)
                <*> many (typeFlag <|> noTypeFlag)
                <*> many letOption
                <*> many (cacheFlag <|> noCacheFlag)
                <*> optional (outputOption @Dhall.Output)
                <*> optional (expressionOption <|> inputOption Dhall.setInput)
            )

    , dhallReplFlag

    , dhallDiffFlag
        <*> Options.strArgument mempty
        <*> Options.strArgument mempty
        <*> ( dualFoldEndo
                <$> optional (outputOption @Dhall.Output)
            )

    , dhallHashFlag
        <*> ( dualFoldEndo
                <$> many (cacheFlag <|> noCacheFlag)
                <*> optional (expressionOption <|> inputOption Dhall.setInput)
                <*> optional (outputOption @Dhall.Output)
            )

    , dhallFreezeFlag
        <*> ( dualFoldEndo
                <$> many (remoteOnlyFlag <|> noRemoteOnlyFlag)
                <*> many (forSecurityFlag <|> forCachingFlag)
                <*> optional (outputOption @Dhall.OutputOrCheck)
                <*> optional (expressionOption <|> inputOption Dhall.setInput)
            )

    , dhallFormatFlag
        <*> ( dualFoldEndo
--              <$> many (checkFlag <|> noCheckFlag)
                <$> optional (outputOption @Dhall.OutputOrCheck)
                <*> optional (expressionOption <|> inputOption Dhall.setInput)
            )

    , dhallLintFlag
        <*> ( dualFoldEndo
                <$> optional (outputOption @Dhall.OutputOrCheck)
                <*> optional (expressionOption <|> inputOption Dhall.setInput)
            )

    , dhallResolveFlag
        <*> ( dualFoldEndo
                <$> many (cacheFlag <|> noCacheFlag)
                <*> optional (outputOption @Dhall.Output)
                <*> optional (expressionOption <|> inputOption Dhall.setInput)
                <*> optional listDependenciesOption
            )

    , dhallBashFlag
        <*> ( dualFoldEndo
                <$> many (allowImportsFlag <|> noAllowImportsFlag)
                <*> many
                    ( secureRemoteImportsImportsFlag
                    <|> noSecureRemoteImportsImportsFlag
                    )
                <*> many (cacheFlag <|> noCacheFlag)
                <*> optional declareOption
                <*> optional (expressionOption <|> inputOption Dhall.setInput)
                <*> optional (outputOption @Dhall.Output)
            )

    , dhallExecFlag
        <*> (expressionOption <|> inputOption Dhall.setInput)
        <*> ( dualFoldEndo
                <$> many (allowImportsFlag <|> noAllowImportsFlag)
                <*> many
                    ( secureRemoteImportsImportsFlag
                    <|> noSecureRemoteImportsImportsFlag
                    )
                <*> many (cacheFlag <|> noCacheFlag)
                <*> optional
                    ( setInterpreter
                        <$> interpreterOption
                        <*> many interpreterArgumentOption
                    )
                <*> many scriptArgument
            )

    , dhallTextFlag
        <*> ( dualFoldEndo
                <$> many (allowImportsFlag <|> noAllowImportsFlag)
                <*> many
                    ( secureRemoteImportsImportsFlag
                    <|> noSecureRemoteImportsImportsFlag
                    )
                <*> many (cacheFlag <|> noCacheFlag)
                <*> many listFlag
                <*> many
                    ( nulFlag \opts ->
                        (opts :: Dhall.ToText){Dhall.outputDelimiter = '\0'}
                    )
                <*> optional (expressionOption <|> inputOption Dhall.setInput)
                <*> optional (outputOption @Dhall.Output)
            )

    , dhallFilterFlag
        <*> Options.strArgument (Options.metavar "EXPRESSION")
        <*> ( dualFoldEndo
                <$> many (allowImportsFlag <|> noAllowImportsFlag)
                <*> many
                    ( secureRemoteImportsImportsFlag
                    <|> noSecureRemoteImportsImportsFlag
                    )
                <*> many (alphaFlag <|> noAlphaFlag)
                <*> many (annotateFlag <|> noAnnotateFlag)
                <*> many (typeFlag <|> noTypeFlag)
                <*> many (cacheFlag <|> noCacheFlag)
                <*> many letOption
                <*> optional (outputOption @Dhall.Output)
                <*> optional (expressionOption <|> inputOption Dhall.setInput)
            )

    , getFlag
        <*> pure (mempty @(Endo GetOptions))
--      <*> ( dualFoldEndo
--              <$> many (allowImportsFlag <|> noAllowImportsFlag)
--              <*> many
--                  ( secureRemoteImportsImportsFlag
--                  <|> noSecureRemoteImportsImportsFlag
--                  )
--              <*> many (alphaFlag <|> noAlphaFlag)
--              <*> many (annotateFlag <|> noAnnotateFlag)
--              <*> many (typeFlag <|> noTypeFlag)
--              <*> many (cacheFlag <|> noCacheFlag)
--              <*> many letOption
--              <*> optional outputOption
--              <*> ( expressionOption
--                  <|> inputOption Dhall.setInput
--                  <|> Options.strArgument (Options.metavar "EXPRESSION")
--                  )
--          )

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

    , Options.helpFlag switchToHelpMode

    , pure mempty
    ]
  where
    switchTo :: ConfigMode Global.Config -> Endo (ConfigMode Global.Config)
    switchTo = Endo . const

    switchToHelpMode :: Endo (ConfigMode Global.Config)
    switchToHelpMode = switchTo (Help globalConfig)

    switchToInitMode :: Endo InitOptions -> Endo (ConfigMode Global.Config)
    switchToInitMode (Endo f) =
        switchTo (Init (f (defInitOptions usedName)) globalConfig)

    switchToGetMode :: Endo GetOptions -> Endo (ConfigMode Global.Config)
    switchToGetMode (Endo f) = switchTo (Get (f defGetOptions) globalConfig)

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

    initFlag
        :: Options.Parser (Endo InitOptions -> Endo (ConfigMode Global.Config))
    initFlag = Options.flag mempty switchToInitMode (Options.long "init")

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

    secureRemoteImportsImportsFlag
        :: HasField' "onlySecureImpors" a Bool => Options.Parser (a -> a)
    secureRemoteImportsImportsFlag =
        Options.flag' (Dhall.setOnlySecureImpors True)
            (Options.long "secure-remote-imports")

    noSecureRemoteImportsImportsFlag
        :: HasField' "onlySecureImpors" a Bool => Options.Parser (a -> a)
    noSecureRemoteImportsImportsFlag =
        Options.flag' (Dhall.setOnlySecureImpors False)
        (Options.long "no-secure-remote-imports")

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
        :: forall o a
        . (Output a ~ o, HasOutput a)
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

    toolsetOption :: Options.Parser (Endo InitOptions)
    toolsetOption =
        Options.toolsetOption Nothing <&> \toolsetName ->
            Endo \opts -> (opts :: InitOptions){toolsetName}

    getFlag
        :: Options.Parser (Endo GetOptions -> Endo (ConfigMode Global.Config))
    getFlag = Options.flag mempty switchToGetMode (Options.long "get")

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

    binDirOption :: (FilePath -> a -> a) -> Options.Parser (Endo a)
    binDirOption f = Options.option parser $ mconcat
        [ Options.long "bin-dir"
        , Options.long "bin-directory"
        , Options.metavar "DIRECTORY"
        ]
      where
        parser = Options.eitherReader \case
            "" -> Left "Expected DIRECTORY, but got empty file path"
            fp -> Right (Endo (f fp))

    configDirOption :: (FilePath -> a -> a) -> Options.Parser (Endo a)
    configDirOption f = Options.option parser $ mconcat
        [ Options.long "config-dir"
        , Options.long "config-directory"
        , Options.metavar "DIRECTORY"
        ]
      where
        parser = Options.eitherReader \case
            "" -> Left "Expected DIRECTORY, but got empty file path"
            fp -> Right (Endo (f fp))

    libexecDirOption :: (FilePath -> a -> a) -> Options.Parser (Endo a)
    libexecDirOption f = Options.option parser $ mconcat
        [ Options.long "libexec-dir"
        , Options.long "libexec-directory"
        , Options.metavar "DIRECTORY"
        ]
      where
        parser = Options.eitherReader \case
            "" -> Left "Expected DIRECTORY, but got empty file path"
            fp -> Right (Endo (f fp))

    manDirOption :: (FilePath -> a -> a) -> Options.Parser (Endo a)
    manDirOption f = Options.option parser $ mconcat
        [ Options.long "man-dir"
        , Options.long "man-directory"
        , Options.metavar "DIRECTORY"
        ]
      where
        parser = Options.eitherReader \case
            "" -> Left "Expected DIRECTORY, but got empty file path"
            fp -> Right (Endo (f fp))

    execParser
        :: [Options.Parser (Endo (ConfigMode Global.Config))]
        -> IO (Endo (ConfigMode Global.Config))
    execParser parser =
        Options.internalSubcommandParse appNames globalConfig "config"
            Options.defaultPrefs (Options.info (asum parser) mempty) options

configSubcommandDescription :: String
configSubcommandDescription =
    "Initialise, query, and update Command Wrapper toolset configuration."

configSubcommandHelp
    :: AppNames
    -> Global.Config
    -> Pretty.Doc (Result Pretty.AnsiStyle)
configSubcommandHelp AppNames{usedName} _config = Pretty.vsep
    [ Pretty.reflow (fromString configSubcommandDescription)
    , ""

    , usageSection usedName
        [ "config"
            <+> longOption "dhall"
            <+> Pretty.brackets (longOption "[no-]allow-imports")
            <+> Pretty.brackets (longOption "[no-]secure-remote-imports")
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
            <+> Pretty.brackets (longOption "[no-]secure-remote-imports")
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
--          <+> Pretty.brackets (longOption "[no-]check")
--          <+> Pretty.brackets (longOptionWithArgument "expected-hash" "HASH")
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
            <+> Pretty.brackets (longOption "[no-]secure-remote-imports")
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
            <+> Pretty.brackets (longOption "[no-]secure-remote-imports")
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
            <+> Pretty.brackets (longOption "[no-]allow-imports")
            <+> Pretty.brackets (longOption "[no-]secure-remote-imports")
            <+> Pretty.brackets (longOption "[no-]cache")
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

        -- TODO: We want config --get to behave more like config --dhall-filter
        -- in the future.
        , "config"
            <+> longOption "get"
--          <+> Pretty.brackets (longOption "[no-]allow-imports")
--          <+> Pretty.brackets (longOption "[no-]secure-remote-imports")
--          <+> Pretty.brackets (longOption "[no-]alpha")
--          <+> Pretty.brackets (longOption "[no-]annotate")
--          <+> Pretty.brackets (longOption "[no-]cache")
--          <+> Pretty.brackets (longOption "[no-]type")
--          <+> Pretty.brackets
--              ( longOptionWithArgument "let" "NAME=EXPRESSION"
--              )
--          <+> Pretty.brackets (longOptionWithArgument "output" "FILE")
--          <+> Pretty.brackets
--              ( longOptionWithArgument "expression" "EXPRESSION"
--              <> "|" <> longOptionWithArgument "input" "FILE"
--              <> "|" <> metavar "EXPRESSION"
--              )

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
            <+> Pretty.brackets (longOptionWithArgument "toolset" "NAME")
            <+> Pretty.brackets
                ( longOptionWithArgument "bin-dir[ectory]" "DIRECTORY"
                )
            <+> Pretty.brackets
                ( longOptionWithArgument "config-dir[ectory]" "DIRECTORY"
                )
            <+> Pretty.brackets
                ( longOptionWithArgument "libexec-dir[ectory]" "DIRECTORY"
                )
            <+> Pretty.brackets
                ( longOptionWithArgument "man-dir[ectory]" "DIRECTORY"
                )

        , "config" <+> helpOptions
        , "help config"
        ]

    , section "Dhall Options:"
        [ optionDescription ["--dhall"]
            [ Pretty.reflow "Run as interpreter for the Dhall language."
            ]

        , optionDescription ["--[no-]allow-imports"]
            [ Pretty.reflow "Controls whether imports in the input expression\
                \ are allowed or not. By default imports are allowed."
            ]

        , optionDescription ["--[no-]secure-remote-imports"]
            [ Pretty.reflow "Controls whether remote imports must be protected\
                \ semantic hash or not. By default most commands allow\
                \ unsecure remote imports. The only exception is"
            , longOption "dhall-exec"
            , Pretty.reflow "which requires secure remote imports by default.\
                \ Be aware that local imports (files and environment variables\
                \ are considered secure. If those are using unsafe sources\
                \ this option won't stop them from doing so."
            ]

        , optionDescription ["--[no-]alpha"]
            [ Pretty.reflow "Perform α-normalisation of Dhall expression. By\
                \ default α-normalisation is not performed."
            ]

        , optionDescription ["--[no-]annotate"]
            [ Pretty.reflow "Add a type annotation to the output. Type\
                \ annotations aren't included by default."
            ]

        , optionDescription ["--[no-]type"]
            [ Pretty.reflow "Print type of final Dhall expression instead of\
                \ its value."
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

        , optionDescription ["EPRESSION"]
            [ "Dhall", metavar "EXPRESSION" <> "."
            ]

        , optionDescription ["ARGUMENT"]
            [ Pretty.reflow "Command line argument passed to executed script in"
            , longOption "dhall-exec", "mode."
            ]
        ]

    , section "Initialise toolset configuation options:"
        [ optionDescription ["--init"]
            [ Pretty.reflow "Initialise configuration of a toolset."
            ]

        , optionDescription ["--toolset=NAME"]
            [ Pretty.reflow "When specified allong with", longOption "init"
            , Pretty.reflow "then configuration for toolset", metavar "NAME"
            , Pretty.reflow "is initialised."
            ]

        , optionDescription ["--bin-dir[ectory]=DIRECTORY"]
            [ Pretty.reflow "When specified allong with", longOption "init"
            , Pretty.reflow "then symbolic link for toolset", metavar "NAME"
            , Pretty.reflow "is created in", metavar "DIRECTORY"
            , Pretty.reflow "instead of trying to create it in"
            , value "~/.local/bin" <> ",", "or"
            , value "~/bin"
            , Pretty.reflow "(directories are tried in that order)."
            ]

        , optionDescription ["--output-dir[ectory]=DIRECTORY"]
            [ Pretty.reflow "When specified allong with", longOption "init"
            , Pretty.reflow "then configuration for toolset", metavar "NAME"
            , Pretty.reflow "is initialised in"
            , metavar "DIRECTORY" <> "/" <> metavar "NAME"
            , Pretty.reflow
                "instead of following XDG Base Directory Specification."
            ]

        , optionDescription ["--libexec-dir[ectory]=DIRECTORY"]
            [ Pretty.reflow "When specified allong with", longOption "init"
            , Pretty.reflow "then it is assumed that subcommand executables\
                \ will be stored in"
            , metavar "DIRECTORY" <> "."
            ]

        , optionDescription ["--man-dir[ectory]=DIRECTORY"]
            [ Pretty.reflow "When specified allong with", longOption "init"
            , Pretty.reflow
                "then it is assumed that manual pages will be stored in"
            , metavar "DIRECTORY" <> "."
            ]
        ]

    , section "Get options:"
        [ optionDescription ["--get"]
            [ Pretty.reflow
                "Get effective configuration of current toolset in the form of\
                \ Dhall expression. Provided value is superset of what is in\
                \ the toolset configuration file and"
            , metavar "GLOBAL_OPTIONS" <> ","
            , Pretty.reflow "such as"
            , longOptionWithArgument "change-directory" "DIRECTORY" <> ","
            , Pretty.reflow "are taken into account."
            ]
        ]

    , section "Edit options:"
        [ optionDescription ["--edit", "-e"]
            [ Pretty.reflow "Start editor to edit", metavar "FILE", "or"
            , metavar "SUBCOMMAND", "when", longOption "subcommand-config"
            , Pretty.reflow "is passed."
            ]

        , optionDescription ["--subcommand-config"]
            [ Pretty.reflow "Open subcommand config when invoked with"
            , longOption "edit" <> "."
            ]
        ]

    , section "Selection menu options:"
        [ optionDescription ["--menu"]
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
        ]

    , section "Other options:"
        [ optionDescription ["--help", "-h"]
            [ Pretty.reflow "Print this help and exit. Same as"
            , Pretty.squotes (toolsetCommand usedName "help config") <> "."
            ]

        , globalOptionsHelp usedName
        ]

    , ""
    ]

configSubcommandCompleter
    :: AppNames
    -> Global.Config
    -> Shell
    -> Word
    -> [String]
    -> IO [String]
configSubcommandCompleter appNames cfg shell index words
  | Just "-o" <- lastMay wordsBeforePattern =
        fileCompleter ""

  | Just "--output" <- lastMay wordsBeforePattern =
        fileCompleter ""

  | Just "-i" <- lastMay wordsBeforePattern =
        fileCompleter ""

  | Just "--input" <- lastMay wordsBeforePattern =
        fileCompleter ""

  | Just "--interpreter" <- lastMay wordsBeforePattern =
        Options.bashCompleter "command" "" pat

  | Bash <- shell
  , Just w <- lastMay wordsBeforePattern
  , isBashRedirection w =
        fileCompleter ""

  | Zsh <- shell
  , Just w <- lastMay wordsBeforePattern
  , isZshRedirection w =
        fileCompleter ""

  | Fish <- shell
  , Just w <- lastMay wordsBeforePattern
  , isFishRedirection w =
        fileCompleter ""

  | shell == Bash || shell == Zsh
  , Just w <- lastMay wordsBeforePattern
  , hadSomeDhall
  , isBashAndZshStdinExpansion w = do
        -- TODO: Very similar code can be found in `exec` (external)
        -- subcommand.  Should this be somewhere in `command-wrapper-core`?
        let envPrefix = "env:"

            hasPathPrefix = List.or
                [ "./" `List.isPrefixOf` pat
                , "../" `List.isPrefixOf` pat
                , "~" `List.isPrefixOf` pat
                ]

        if
          | envPrefix `List.isPrefixOf` pat ->
                -- Syntax `env:VARIABLE` is a valid import in Dhall.
                environmentVariablesCompleter defEnvironmentVariablesOptions
                    { word = List.drop (length envPrefix) pat
                    , prefix = envPrefix
                    }

          | pat == "." || pat == ".." ->
                pure (List.filter (pat `List.isPrefixOf`) ["./", "../"])

          | hasPathPrefix ->
                -- File paths, even `~/some/path`, are valid Dhall expressions.
                -- However, relative paths like `foo/bar` are not, they need to
                -- start with `./` or `../`
                fileCompleter ""

          | otherwise ->
                let alternatives = ["./", "../", "~/", "env:"]
                in pure (List.filter (pat `List.isPrefixOf`) alternatives)

  | Just "--edit" <- lastMay wordsBeforePattern, '-' : _ <- pat =
        pure
            ( List.filter (fmap Char.toLower pat `List.isPrefixOf`)
                ["--subcommand-config"]
            )

  | Just "--edit" <- lastMay wordsBeforePattern =
        fileCompleter ""

  | Just "-e" <- lastMay wordsBeforePattern =
        fileCompleter ""

  | Just "--subcommand-config" <- lastMay wordsBeforePattern =
        List.filter (fmap Char.toLower pat `List.isPrefixOf`)
            <$> External.findSubcommands appNames cfg

  | "--output=" `List.isPrefixOf` pat =
        fileCompleter "--output="

  | "--input=" `List.isPrefixOf` pat =
        fileCompleter "--input="

  | "--interpreter=" `List.isPrefixOf` pat =
        Options.bashCompleter "command" "--interpreter=" pat

{-
  | "--let=" `List.isPrefixOf` pat =
        TODO: Figure out if it contains another '=' sign to do completion on
        file.
        fsCompleter "file" ("--let=" <> variableName <> "=")
-}

  | null pat =
        pure possibleOptions

  | otherwise =
        pure matchingOptions
  where
    wordsBeforePattern = List.take (fromIntegral index) words
    pat = fromMaybe "" $ atMay words (fromIntegral index)

--  hadGet = ("--get" `List.elem` wordsBeforePattern)

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

    hadBinDir = List.or
        [ "--bin-dir" `List.elem` wordsBeforePattern
        , "--bin-directory" `List.elem` wordsBeforePattern
        , List.any ("--bin-dir=" `List.isPrefixOf`) wordsBeforePattern
        , List.any ("--bin-directory=" `List.isPrefixOf`) wordsBeforePattern
        ]

    hadConfigDir = List.or
        [ "--config-dir" `List.elem` wordsBeforePattern
        , "--config-directory" `List.elem` wordsBeforePattern
        , List.any ("--config-dir=" `List.isPrefixOf`) wordsBeforePattern
        , List.any ("--config-directory=" `List.isPrefixOf`) wordsBeforePattern
        ]

    hadLibexecDir = List.or
        [ "--libexec-dir" `List.elem` wordsBeforePattern
        , "--libexec-directory" `List.elem` wordsBeforePattern
        , List.any ("--libexec-dir=" `List.isPrefixOf`) wordsBeforePattern
        , List.any ("--libexec-directory=" `List.isPrefixOf`) wordsBeforePattern
        ]

    hadManDir = List.or
        [ "--man-dir" `List.elem` wordsBeforePattern
        , "--man-directory" `List.elem` wordsBeforePattern
        , List.any ("--man-dir=" `List.isPrefixOf`) wordsBeforePattern
        , List.any ("--man-directory=" `List.isPrefixOf`) wordsBeforePattern
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
            , "--get"
            , "--help", "-h"
            , "--init"
            , "--menu"
            ]
        <> mwhen (hadInit && not hadToolset) ["--toolset="]
        <> mwhen (hadInit && not hadBinDir) ["--bin-dir="]
        <> mwhen (hadInit && not hadConfigDir) ["--config-dir="]
        <> mwhen (hadInit && not hadLibexecDir) ["--libexec-dir="]
        <> mwhen (hadInit && not hadManDir) ["--man-dir="]
        <> mwhen (hadDhall || hadDhallFilter)
            [ "--alpha", "--no-alpha"
            , "--annotate", "--no-annotate"
            , "--type", "--no-type"
            ]
        <> munless
            ( List.or
                [ hadDhallDiff
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
                    , hadDhallExec
                    ]
                ]
            )
            [ "--allow-imports", "--no-allow-imports"
            , "--secure-remote-imports", "--no-secure-remote-imports"
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
                    , hadDhallExec
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
        ]

    -- See <http://zsh.sourceforge.net/Doc/Release/Redirection.html>.
    isZshRedirection s = List.dropWhile Char.isDigit s `List.elem`
        [ "<"
        , "<<<"
        , "<>"
        , ">!"
        , ">"
        , ">&"
        , ">>"
        , ">>&"
        , ">|"
        ]

    -- This expansion is available in both Bash and Zsh.
    isBashAndZshStdinExpansion s = List.dropWhile Char.isDigit s == "<<<"

    -- There's probably more than this. Following list is based on:
    -- <https://fishshell.com/docs/current/index.html#syntax>
    isFishRedirection s = List.dropWhile Char.isDigit s `List.elem`
        [ "<"
        , ">"
        , ">>"
        , ">?"
        ]

    fileCompleter prefix =
        fileSystemCompleter defFileSystemOptions
            { appendSlashToSingleDirectoryResult = True
            , expandTilde = not (null prefix)
            , prefix
            , word = List.drop (length prefix) pat
            }
