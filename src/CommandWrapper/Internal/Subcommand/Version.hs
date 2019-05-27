{-# LANGUAGE PatternSynonyms #-}
-- |
-- Module:      CommandWrapper.Internal.Version
-- Description: Implementation of internal command named version
-- Copyright:   (c) 2018-2019 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Implementation of internal command named @version@.
module CommandWrapper.Internal.Subcommand.Version
    ( VersionInfo(..)
    , VersionMode(..)
    , PrettyVersion(..)
    , version
    , versionSubcommandHelp
    , versionQQ
    , versionCompletion
    )
  where

import Control.Applicative ((<|>), optional)
import Data.Bool ((||), not, otherwise)
import Data.Foldable (null)
import Data.Functor ((<&>))
import qualified Data.List as List (elem, filter, isPrefixOf)
import Data.Maybe (fromMaybe)
import Data.Monoid (Endo(Endo, appEndo))
import Data.String (fromString)
import Data.Version (showVersion)
import GHC.Generics (Generic)
import System.IO (Handle, IOMode(WriteMode), stdout, withFile)

import Data.Monoid.Endo.Fold (foldEndo)
import Data.Output
    ( OutputFile(OutputFile)
    , OutputHandle(OutputHandle, OutputNotHandle)
    , OutputStdoutOrFile
    , pattern OutputStdoutOnly
    )
import qualified Data.Output.Colour as ColourOutput (ColourOutput(Auto))
import Data.Text (Text)
import qualified Data.Text as Text (unlines)
import qualified Data.Text.IO as Text (hPutStr)
import Data.Text.Prettyprint.Doc (Pretty(pretty), (<+>))
import qualified Data.Text.Prettyprint.Doc as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Pretty (AnsiStyle)
import qualified Data.Text.Prettyprint.Doc.Util as Pretty (reflow)
import qualified Dhall
import qualified Dhall.Pretty as Dhall (CharacterSet(Unicode))
import qualified Mainplate (applySimpleDefaults)
import qualified Options.Applicative as Options
    ( Parser
    , defaultPrefs
    , flag
    , info
    , long
    , short
    )
import qualified Options.Applicative.Standard as Options (outputOption)
import Safe (headMay, lastMay)

import CommandWrapper.Config.Global (Config(..))
import CommandWrapper.Environment (AppNames(AppNames, usedName))
--import qualified CommandWrapper.External as External (executeCommand)
import CommandWrapper.Internal.Dhall as Dhall (hPut)
import CommandWrapper.Internal.Subcommand.Help
    ( globalOptionsHelp
    , helpOptions
    , longOption
    , longOptionWithArgument
    , metavar
    , optionDescription
    , section
    , shortOption
    , toolsetCommand
    , usageSection
    )
import CommandWrapper.Internal.Subcommand.Version.Info
    ( PrettyVersion(..)
    , VersionInfo(..)
    , VersionInfoField(..)
    , versionQQ
    )
import CommandWrapper.Internal.Utils (runMain)
import CommandWrapper.Message (Result, defaultLayoutOptions, message)
--import qualified CommandWrapper.Message as Message (dieTooManyArguments)
--import CommandWrapper.Options.Alias (applyAlias)
import qualified CommandWrapper.Options.Optparse as Options
    ( bashCompleter
    , internalSubcommandParse
    )
import qualified CommandWrapper.Options.Shell as Options


data VersionMode a
    = FullVersion OutputFormat OutputStdoutOrFile a
    | NumericVersion (Maybe VersionInfoField) OutputStdoutOrFile a
    | VersionHelp a
  deriving stock (Functor, Generic, Show)

data OutputFormat
    = PlainFormat
    | DhallFormat
    | ShellFormat Options.Shell
  deriving stock (Generic, Show)

version
    :: VersionInfo
    -> AppNames
    -> [String]
    -> Config
    -> IO ()
version versionInfo appNames options config =
    runMain (parseOptions appNames config options) defaults $ \case
        FullVersion format output _config ->
            withOutputHandle output \handle -> case format of
                DhallFormat ->
                    Dhall.hPut colour Dhall.Unicode handle Dhall.inject
                        versionInfo

                ShellFormat shell ->
                    (\f -> Text.hPutStr handle $ f versionInfo) case shell of
                        Options.Bash -> versionInfoBash
                        Options.Fish -> versionInfoFish
                        Options.Zsh -> versionInfoBash -- Same syntax as Bash.

                PlainFormat ->
                    message defaultLayoutOptions verbosity colour handle
                        (versionInfoDoc versionInfo)

        NumericVersion _field output _config ->
            withOutputHandle output \handle ->
                message defaultLayoutOptions verbosity colour handle
                    ("TODO" :: Pretty.Doc (Result Pretty.AnsiStyle))

        VersionHelp _config ->
            message defaultLayoutOptions verbosity colour stdout
                (versionSubcommandHelp appNames)
  where
    defaults = Mainplate.applySimpleDefaults
        (FullVersion PlainFormat OutputStdoutOnly ())

    Config{colourOutput, verbosity} = config
    colour = fromMaybe ColourOutput.Auto colourOutput

    withOutputHandle :: OutputStdoutOrFile -> (Handle -> IO a) -> IO a
    withOutputHandle = \case
        OutputHandle _ -> ($ stdout)
        OutputNotHandle (OutputFile fn) -> withFile fn WriteMode

versionInfoDoc :: VersionInfo -> Pretty.Doc (Result Pretty.AnsiStyle)
versionInfoDoc VersionInfo{..} = Pretty.vsep
    [ "Command Wrapper Tool:" <+> pretty commandWrapper
    , "Subcommand Protocol: " <+> pretty subcommandProtocol
    , "Dhall Library:       " <+> pretty dhallLibrary
    , "Dhall Standard:      " <+> pretty dhallStandard
    , ""
    ]

versionInfoBash :: VersionInfo -> Text
versionInfoBash VersionInfo{..} = Text.unlines
    [ var "TOOL" (showVersion' commandWrapper)
    , var "SUBCOMMAND_PROTOCOL" (showVersion' subcommandProtocol)
    , var "DHALL_LIBRARY" (showVersion' dhallLibrary)
    , var "DHALL_STANDARD" (showVersion' dhallStandard)
    ]
  where
    showVersion' = fromString . showVersion . rawVersion

    var n v = "COMMAND_WRAPPER_" <> n <> "_VERSION='" <> v <> "'"

versionInfoFish :: VersionInfo -> Text
versionInfoFish VersionInfo{..} = Text.unlines
    [ var "TOOL" (showVersion' commandWrapper)
    , var "SUBCOMMAND_PROTOCOL" (showVersion' subcommandProtocol)
    , var "DHALL_LIBRARY" (showVersion' dhallLibrary)
    , var "DHALL_STANDARD" (showVersion' dhallStandard)
    ]
  where
    showVersion' = fromString . showVersion . rawVersion

    var n v = "set COMMAND_WRAPPER_" <> n <> "_VERSION '" <> v <> "'"

parseOptions :: AppNames -> Config -> [String] -> IO (Endo (VersionMode ()))
parseOptions appNames config options =
    execParser $ foldEndo
        <$> (   dhallFlag
            <|> shellOption
            <|> helpFlag
            )
        <*> optional outputOption
  where
    switchTo f = Endo \case
        FullVersion _ o a -> f o a
        NumericVersion _ o a -> f o a
        VersionHelp a -> f OutputStdoutOnly a

    switchToDhallFormat = switchTo (FullVersion DhallFormat)
    switchToHelpMode = switchTo (const VersionHelp)

    switchToShellFormat f =
        let shell = f `appEndo` Options.Bash
        in  switchTo (FullVersion (ShellFormat shell))

    dhallFlag, shellOption, helpFlag :: Options.Parser (Endo (VersionMode ()))

    dhallFlag = Options.flag mempty switchToDhallFormat (Options.long "dhall")

    shellOption = switchToShellFormat <$> Options.shellOption

    outputOption :: Options.Parser (Endo (VersionMode ()))
    outputOption = Options.outputOption <&> \o -> Endo \case
        FullVersion format _ a -> FullVersion format o a
        NumericVersion field _ a -> NumericVersion field o a
        VersionHelp a -> VersionHelp a

    execParser parser =
        Options.internalSubcommandParse appNames config "version"
            Options.defaultPrefs (Options.info parser mempty) options

    helpFlag = Options.flag mempty switchToHelpMode $ mconcat
        [ Options.short 'h'
        , Options.long "help"
        ]

versionSubcommandHelp :: AppNames -> Pretty.Doc (Result Pretty.AnsiStyle)
versionSubcommandHelp AppNames{usedName} = Pretty.vsep
    [ Pretty.reflow "Display version information."
    , ""

    , usageSection usedName
        [ "version"
            <+> Pretty.brackets
                ( longOption "dhall"
                <> "|" <> longOptionWithArgument "shell" "SHELL"
--              <> "|" <> longOptionWithArgument "numeric" "COMPONENT"
                )
            <+> Pretty.brackets (longOption "output" <> "=" <> metavar "FILE")
        , "version" <+> helpOptions
        , "help version"
        , Pretty.braces (longOption "version" <> "|" <> shortOption 'V')
        ]

    , section "Options:"
        [ optionDescription ["--dhall"]
            [ Pretty.reflow "Print version information in Dhall format."
            ]

        , optionDescription ["--shell=SHELL"]
            [ Pretty.reflow
                "Print version information in format suitable for SHELL."
            ]

        , optionDescription ["--output=FILE", "--output FILE", "-o FILE"]
            [ Pretty.reflow "Write optput into", metavar "FILE"
            , Pretty.reflow "instead of standard output."
            ]

--      , optionDescription ["--numeric=COMPONENT"]
--          [ Pretty.reflow "Print version of ", metavar "COMPONENT"
--          , Pretty.reflow " in machine readable form."
--          ]

        , optionDescription ["--help", "-h"]
            [ Pretty.reflow "Print this help and exit. Same as"
            , Pretty.squotes (toolsetCommand usedName "help version") <> "."
            ]

        , globalOptionsHelp usedName
        ]

    , ""
    ]

versionCompletion
    :: AppNames
    -> Config
    -> [String]
    -> String
    -> IO [String]
versionCompletion _ _ wordsBeforePattern pat
  | Just "-o" <- lastMay wordsBeforePattern =
        bashCompleter "file" ""

  | Just "--output" <- lastMay wordsBeforePattern =
        bashCompleter "file" ""

  | null pat =
        pure versionOptions

  | "--shell=" `List.isPrefixOf` pat =
        pure $ List.filter (pat `List.isPrefixOf`) shellOptions

  | "--output=" `List.isPrefixOf` pat =
        bashCompleter "file" "--output="

  | Just '-' <- headMay pat =
        pure case List.filter (pat `List.isPrefixOf`) versionOptions of
            ["--shell="] -> shellOptions
            opts -> opts

  | otherwise =
        pure []
  where
    hadHelp =
        ("--help" `List.elem` wordsBeforePattern)
        || ("-h" `List.elem` wordsBeforePattern)

    hadDhall = "--dhall" `List.elem` wordsBeforePattern

    hadShell =
        not . null
        $ List.filter ("--shell=" `List.isPrefixOf`) wordsBeforePattern

    versionOptions =
        munless (hadDhall || hadShell || hadHelp)
            ["--help", "-h", "--dhall", "--shell="]
        <> munless hadHelp ["-o", "--output="]

    shellOptions = ("--shell=" <>) <$> ["bash", "fish"{-, "zsh"-}]

    munless p x = if not p then x else mempty

    bashCompleter a p = Options.bashCompleter a p pat
