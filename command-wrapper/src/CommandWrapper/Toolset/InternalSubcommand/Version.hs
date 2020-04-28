-- |
-- Module:      $Header$
-- Description: Implementation of internal command named version
-- Copyright:   (c) 2018-2020 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Implementation of internal command named @version@.
module CommandWrapper.Toolset.InternalSubcommand.Version
    ( VersionInfo(..)
    , PrettyVersion(..)
    , version
    , versionQQ
    , versionSubcommandCompleter
    , versionSubcommandDescription
    , versionSubcommandHelp
    )
  where

import Prelude (fromIntegral)

import Control.Applicative ((<*>), (<|>), optional, pure)
import Data.Bool (Bool(True), (||), not, otherwise)
import Data.Foldable (length, null)
import Data.Function (($), (.), const)
import Data.Functor (Functor, (<$>), (<&>), fmap)
import qualified Data.List as List (drop, elem, filter, isPrefixOf, take)
import Data.Maybe (Maybe(Just), fromMaybe)
import Data.Monoid (Endo(Endo, appEndo), mempty)
import Data.Semigroup ((<>))
import Data.String (String, fromString)
import Data.Version (showVersion)
import Data.Word (Word)
import GHC.Generics (Generic)
import System.IO (Handle, IO, IOMode(WriteMode), stdout, withFile)
import Text.Show (Show)

import Data.Monoid.Endo.Fold (foldEndo)
import Data.Output
    ( OutputFile(OutputFile)
    , OutputHandle(OutputHandle, OutputNotHandle)
    , OutputStdoutOrFile
    , pattern OutputStdoutOnly
    )
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
import qualified Options.Applicative as Options (Parser, defaultPrefs, info)
import qualified Options.Applicative.Standard as Options (outputOption)
import Safe (atMay, headMay, lastMay)

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
--import CommandWrapper.Core.Config.Alias (applyAlias)
import CommandWrapper.Core.Config.Shell (Shell(Bash, Fish, Zsh))
import qualified CommandWrapper.Core.Config.Shell as Shell (shellOption)
import CommandWrapper.Core.Dhall as Dhall (hPut)
import CommandWrapper.Core.Environment (AppNames(AppNames, usedName))
import CommandWrapper.Core.Help.Pretty
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
import CommandWrapper.Core.Message (Result, out)
import CommandWrapper.Toolset.Config.Global
    ( Config(Config, colourOutput, verbosity)
    )
import CommandWrapper.Toolset.InternalSubcommand.Utils (runMain)
import qualified CommandWrapper.Toolset.InternalSubcommand.Utils as Options
    ( dhallFlag
    , helpFlag
    )
import CommandWrapper.Toolset.InternalSubcommand.Version.Info
    ( PrettyVersion(..)
    , VersionInfo(..)
    , VersionInfoField(..)
    , versionQQ
    )
import qualified CommandWrapper.Toolset.Options.Optparse as Options
    ( internalSubcommandParse
    )


data VersionMode a
    = FullVersion OutputFormat OutputStdoutOrFile a
    | NumericVersion (Maybe VersionInfoField) OutputStdoutOrFile a
    | VersionHelp a
  deriving stock (Functor, Generic, Show)

data OutputFormat
    = PlainFormat
    | DhallFormat
    | ShellFormat Shell
  deriving stock (Generic, Show)

version
    :: VersionInfo
    -> AppNames
    -> [String]
    -> Config
    -> IO ()
version versionInfo appNames options config =
    runMain (parseOptions appNames config options) defaults \case
        FullVersion format output Config{colourOutput, verbosity} -> do
            withOutputHandle output \handle -> case format of
                DhallFormat ->
                    Dhall.hPut colourOutput Dhall.Unicode handle Dhall.inject
                        versionInfo

                ShellFormat shell ->
                    (\f -> Text.hPutStr handle $ f versionInfo) case shell of
                        Bash -> versionInfoBash
                        Fish -> versionInfoFish
                        Zsh -> versionInfoBash -- Same syntax as Bash.

                PlainFormat ->
                    out verbosity colourOutput handle
                        (versionInfoDoc versionInfo)

        NumericVersion _field output Config{colourOutput, verbosity} -> do
            withOutputHandle output \handle ->
                out verbosity colourOutput handle "TODO: Implement"

        VersionHelp config'@Config{colourOutput, verbosity} -> do
            out verbosity colourOutput stdout
                (versionSubcommandHelp appNames config')
  where
    defaults = Mainplate.applySimpleDefaults
        (FullVersion PlainFormat OutputStdoutOnly config)

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

parseOptions :: AppNames -> Config -> [String] -> IO (Endo (VersionMode Config))
parseOptions appNames config options =
    execParser $ foldEndo
        <$> (   Options.dhallFlag switchToDhallFormat
            <|> fmap switchToShellFormat Shell.shellOption
            <|> Options.helpFlag switchToHelpMode
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
        let shell = f `appEndo` Bash
        in  switchTo (FullVersion (ShellFormat shell))

    outputOption :: Options.Parser (Endo (VersionMode Config))
    outputOption = Options.outputOption <&> \o -> Endo \case
        FullVersion format _ a -> FullVersion format o a
        NumericVersion field _ a -> NumericVersion field o a
        VersionHelp a -> VersionHelp a

    execParser parser =
        Options.internalSubcommandParse appNames config "version"
            Options.defaultPrefs (Options.info parser mempty) options

versionSubcommandDescription :: String
versionSubcommandDescription = "Display version information."

versionSubcommandHelp
    :: AppNames
    -> Config
    -> Pretty.Doc (Result Pretty.AnsiStyle)
versionSubcommandHelp AppNames{usedName} _config = Pretty.vsep
    [ Pretty.reflow (fromString versionSubcommandDescription)
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

versionSubcommandCompleter
    :: AppNames
    -> Config
    -> Shell
    -> Word
    -> [String]
    -> IO [String]
versionSubcommandCompleter _appNames _config _shell index words
  | Just "-o" <- lastMay wordsBeforePattern =
        fsCompleter ""

  | Just "--output" <- lastMay wordsBeforePattern =
        fsCompleter ""

  | null pat =
        pure versionOptions

  | "--shell=" `List.isPrefixOf` pat =
        pure $ List.filter (pat `List.isPrefixOf`) shellOptions

  | "--output=" `List.isPrefixOf` pat =
        fsCompleter "--output="

  | Just '-' <- headMay pat =
        pure case List.filter (pat `List.isPrefixOf`) versionOptions of
            ["--shell="] -> shellOptions
            opts -> opts

  | otherwise =
        pure []
  where
    wordsBeforePattern = List.take (fromIntegral index) words
    pat = fromMaybe "" $ atMay words (fromIntegral index)

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

    shellOptions = ("--shell=" <>) <$> ["bash", "fish", "zsh"]

    munless p x = if not p then x else mempty

    fsCompleter prefix =
        fileSystemCompleter defFileSystemOptions
            { appendSlashToSingleDirectoryResult = True
            , expandTilde = not (null prefix)
            , prefix
            , word = List.drop (length prefix) pat
            }
