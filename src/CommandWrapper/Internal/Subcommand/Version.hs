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
    )
  where

import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe)
import Data.Monoid (Endo(Endo))
import Data.String (fromString)
import Data.Version (showVersion)
import GHC.Generics (Generic)
import System.IO (stdout)

import Data.Monoid.Endo.Fold (foldEndo)
import qualified Data.Output.Colour as ColourOutput (ColourOutput(Auto))
import Data.Text (Text)
import qualified Data.Text as Text (unlines)
import qualified Data.Text.IO as Text (putStr)
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

import CommandWrapper.Config.Global (Config(..))
import CommandWrapper.Environment (AppNames(AppNames, usedName))
--import qualified CommandWrapper.External as External (executeCommand)
import CommandWrapper.Internal.Dhall as Dhall (hPut)
import CommandWrapper.Internal.Subcommand.Help
    ( globalOptionsHelp
    , helpOptions
    , longOption
--  , metavar
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
    ( internalSubcommandParse
    )


data VersionMode a
    = FullVersion OutputFormat a
    | NumericVersion (Maybe VersionInfoField) a
    | VersionHelp a
  deriving stock (Functor, Generic, Show)

data OutputFormat = PlainFormat | DhallFormat | BashFormat
  deriving stock (Generic, Show)

version
    :: VersionInfo
    -> AppNames
    -> [String]
    -> Config
    -> IO ()
version versionInfo appNames options config =
    runMain (parseOptions appNames config options) defaults $ \case
        FullVersion format _config -> case format of
            DhallFormat ->
                Dhall.hPut colour Dhall.Unicode stdout Dhall.inject versionInfo

            BashFormat ->
                Text.putStr (versionInfoBash versionInfo)

            PlainFormat ->
                message defaultLayoutOptions verbosity colour stdout
                    (versionInfoDoc versionInfo)

        NumericVersion _field _config ->
            message defaultLayoutOptions verbosity colour stdout
                ("TODO" :: Pretty.Doc (Result Pretty.AnsiStyle))

        VersionHelp _config ->
            message defaultLayoutOptions verbosity colour stdout
                (versionSubcommandHelp appNames)
  where
    defaults = Mainplate.applySimpleDefaults (FullVersion PlainFormat ())

    Config{colourOutput, verbosity} = config
    colour = fromMaybe ColourOutput.Auto colourOutput

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

    var n v = "COMMAND_WRAPPER_" <> n <> "_VERSION=\"" <> v <> "\""

parseOptions :: AppNames -> Config -> [String] -> IO (Endo (VersionMode ()))
parseOptions appNames config options =
    execParser $ foldEndo
        <$> (   dhallFlag
            <|> bashFlag
            <|> helpFlag
            )

  where
    switchTo = Endo . const
    switchToBashFormat = switchTo (FullVersion BashFormat ())
    switchToDhallFormat = switchTo (FullVersion DhallFormat ())
    switchToHelpMode = switchTo (VersionHelp ())

    dhallFlag, bashFlag, helpFlag :: Options.Parser (Endo (VersionMode ()))

    dhallFlag = Options.flag mempty switchToDhallFormat (Options.long "dhall")

    bashFlag = Options.flag mempty switchToBashFormat (Options.long "bash")

    execParser parser =
        Options.internalSubcommandParse appNames config "version"
            Options.defaultPrefs (Options.info parser mempty) options

    helpFlag = Options.flag mempty switchToHelpMode $ mconcat
        [ Options.short 'h'
        , Options.long "help"
        ]

versionSubcommandHelp :: AppNames -> Pretty.Doc (Result Pretty.AnsiStyle)
versionSubcommandHelp AppNames{usedName} = Pretty.vsep
    [ usageSection usedName
        [ "version" <+> Pretty.brackets
            ( longOption "dhall"
            <> "|" <> longOption "bash"
--          <> "|" <> longOption "numeric"
--              <> Pretty.brackets ("=" <> metavar "COMPONENT")
            )
        , "version" <+> helpOptions
        , "help version"
        , Pretty.braces (longOption "version" <> "|" <> shortOption 'V')
        ]

    , section "Options:"
        [ optionDescription ["--dhall"]
            [ Pretty.reflow "Print version information in Dhall format."
            ]

        , optionDescription ["--bash"]
            [ Pretty.reflow
                "Print version information in format suitable for Bash."
            ]

--      , optionDescription ["--numeric[=COMPONENT]"]
--          [ "Print version of COMPONENT in machine readable form."
--          ]

        , optionDescription ["--help", "-h"]
            [ Pretty.reflow "Print this help and exit. Same as"
            , Pretty.squotes (toolsetCommand usedName "help version") <> "."
            ]

        , globalOptionsHelp usedName
        ]

    , ""
    ]
