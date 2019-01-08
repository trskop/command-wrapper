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
    )
  where

import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe)
import Data.Monoid (Endo(Endo))
import Data.String (fromString)
import Data.Version (Version(versionBranch), showVersion)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import System.IO (stdout)

import Data.Monoid.Endo.Fold (foldEndo)
import qualified Data.Output.Colour as ColourOutput (ColourOutput(Auto))
import Data.Text (Text)
import qualified Data.Text as Text (unlines)
import qualified Data.Text.IO as Text (putStr)
import Data.Text.Prettyprint.Doc (Pretty(pretty), (<+>))
import qualified Data.Text.Prettyprint.Doc as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Pretty (AnsiStyle)
import qualified Dhall
import qualified Dhall.Pretty as Dhall (CharacterSet(Unicode))
import qualified Mainplate (applySimpleDefaults)
import qualified Options.Applicative as Options
    ( Parser
    , defaultPrefs
    , flag
    , help
    , info
    , long
    )

import CommandWrapper.Config.Global (Config(..))
import CommandWrapper.Environment (AppNames(AppNames, usedName))
--import qualified CommandWrapper.External as External (executeCommand)
import CommandWrapper.Internal.Dhall as Dhall (hPut)
import CommandWrapper.Internal.Subcommand.Help
    ( {-globalOptionsSection
    ,-} option
    , section
    , usageSection
    )
import CommandWrapper.Internal.Utils (runMain)
import CommandWrapper.Message (Result, defaultLayoutOptions, message)
--import qualified CommandWrapper.Message as Message (dieTooManyArguments)
--import CommandWrapper.Options.Alias (applyAlias)
import qualified CommandWrapper.Options.Optparse as Options
    ( execParserPure
    , handleParseResult
    )


data VersionMode a
    = FullVersion OutputFormat a
    | NumericVersion (Maybe VersionInfoField) a
  deriving stock (Functor, Generic, Show)

data OutputFormat = PlainFormat | DhallFormat | BashFormat
  deriving stock (Generic, Show)

newtype PrettyVersion = PrettyVersion {rawVersion :: Version}
  deriving stock (Generic, Show)

instance Pretty PrettyVersion where
    pretty (PrettyVersion v) = pretty (showVersion v)

instance Dhall.Inject PrettyVersion where
    injectWith opts = Dhall.InputType
        { embed = embedList . toNaturals . versionBranch . rawVersion
        , declared
        }
      where
        Dhall.InputType{embed = embedList, declared} = Dhall.injectWith opts

        toNaturals :: [Int] -> [Natural] = fmap fromIntegral

data VersionInfo = VersionInfo
    { commandWrapper :: PrettyVersion
    , subcommandProtocol :: PrettyVersion
    , dhallLibrary :: PrettyVersion
    , dhallStandard :: PrettyVersion
    }
  deriving stock (Generic, Show)
  deriving anyclass (Dhall.Inject)

data VersionInfoField
    = CommandWrapperField
    | SubcommandProtocolField
    | DhallLibrary
    | DhallStandard
  deriving stock (Eq, Generic, Ord, Show)

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
  where
    defaults = Mainplate.applySimpleDefaults (FullVersion PlainFormat ())

    Config{colourOutput, verbosity} = config
    colour = fromMaybe ColourOutput.Auto colourOutput

versionInfoDoc :: VersionInfo -> Pretty.Doc (Result Pretty.AnsiStyle)
versionInfoDoc VersionInfo{..} = Pretty.vsep
    [ "Command Wrapper Tool:" <+> pretty commandWrapper
    , "Subcommand Protocol:" <+> pretty subcommandProtocol
    , "Dhall Library:" <+> pretty dhallLibrary
    , "Dhall Standard:" <+> pretty dhallStandard
    , ""
    ]

versionInfoBash :: VersionInfo -> Text
versionInfoBash VersionInfo{..} = Text.unlines
    [ var "TOOL" commandWrapper
    , var "SUBCOMMAND_PROTOCOL" subcommandProtocol
    , var "DHALL_LIBRARY" dhallLibrary
    , var "DHALL_STANDARD" dhallStandard
    ]
  where
    var n v =
        "COMMAND_WRAPPER_" <> n <> "_VERSION=\""
        <> fromString (showVersion (rawVersion v))
        <> "\""

parseOptions :: AppNames -> Config -> [String] -> IO (Endo (VersionMode ()))
parseOptions appNames _config options =
    execParser $ foldEndo
        <$> (dhallFlag <|> bashFlag)
  where
    switchTo = Endo . const
    switchToBashFormat = switchTo (FullVersion BashFormat ())
    switchToDhallFormat = switchTo (FullVersion DhallFormat ())

    dhallFlag, bashFlag :: Options.Parser (Endo (VersionMode ()))

    dhallFlag = Options.flag mempty switchToDhallFormat $ mconcat
        [ Options.long "dhall"
        , Options.help "Print version information in Dhall format."
        ]

    bashFlag = Options.flag mempty switchToBashFormat $ mconcat
        [ Options.long "bash"
        , Options.help "Print version information in format suitable for Bash."
        ]

    execParser parser =
        Options.handleParseResult appNames
            $ Options.execParserPure Options.defaultPrefs
                (Options.info parser mempty) options

versionSubcommandHelp :: AppNames -> Pretty.Doc (Result Pretty.AnsiStyle)
versionSubcommandHelp AppNames{usedName} = Pretty.vsep
    [ usageSection usedName
--      [ "[GLOBAL_OPTIONS] version [--dhall|--bash|--numeric[=COMPONENT]]"
        [ "[GLOBAL_OPTIONS] version [--dhall|--bash]"
        , "[GLOBAL_OPTIONS] {--version|-V}"
        ]

    , section "Options:"
        [ option ["--dhall"]
            [ "Print version information in Dhall format."
            ]

        , option ["--bash"]
            [ "Print version information in format suitable for Bash."
            ]

--      , option ["--numeric[=COMPONENT]"]
--          [ "Print version of COMPONENT in machine readable form."
--          ]
--
--      , option ["--help", "-h"]
--          [ "Print this help and exit.  Same as '... help version'"
--          ]
        ]
    ]
