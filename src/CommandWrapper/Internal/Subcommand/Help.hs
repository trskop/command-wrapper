-- |
-- Module:      CommandWrapper.Internal.Help
-- Description: Implementation of internal command named help
-- Copyright:   (c) 2018-2019 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions; POSIX.
--
-- Implementation of internal command named @help@.
module CommandWrapper.Internal.Subcommand.Help
    ( HelpMode(..)
    , help
    , helpSubcommandHelp

    -- * Utilities
    , (<++>)
    , command
    , dullGreen
    , globalOptions
    , globalOptionsHelp
    , helpOptions
    , longOption
    , metavar
    , optionDescription
    , optionalMetavar
    , optionalSubcommand
    , section
    , shortOption
    , toolsetCommand
    , usageSection
    , value
    )
  where

import Control.Applicative ((<|>), optional)
import Data.Foldable (traverse_)
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import Data.Monoid (Endo(Endo))
import GHC.Generics (Generic)
import System.IO (stdout)

import Data.Monoid.Endo.Fold (foldEndo)
import Data.Text (Text)
import qualified Data.Output.Colour as ColourOutput (ColourOutput(Auto))
import Data.Text.Prettyprint.Doc (Pretty(pretty), (<+>))
import qualified Data.Text.Prettyprint.Doc as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Pretty --(AnsiStyle)
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

import CommandWrapper.Config.Global (Config(..))
import CommandWrapper.Environment (AppNames(AppNames, usedName))
import qualified CommandWrapper.External as External (executeCommand)
import CommandWrapper.Internal.Utils (runMain)
import CommandWrapper.Message (Result(..), defaultLayoutOptions, message)
import CommandWrapper.Options.Alias (applyAlias)
import qualified CommandWrapper.Options.Optparse as Options
    ( internalSubcommandParse
    )


data HelpMode a
    = MainHelp a
    | SubcommandHelp String a
    | ManPage String a
  deriving stock (Functor, Generic, Show)

help
    :: (String -> Maybe (Pretty.Doc (Result Pretty.AnsiStyle)))
    -- ^ Return help message to print if string argument is an internal
    -- subcommand.
    --
    -- TODO: Return something more structured.
    -> AppNames
    -> [String]
    -> Config
    -> IO ()
help internalHelp appNames options config =
    runMain (parseOptions appNames config options) defaults $ \case
        MainHelp _config -> do
            message defaultLayoutOptions verbosity colour stdout
                (mainHelpMsg appNames)
            traverse_ putStrLn extraHelpMessage

        SubcommandHelp cmd _config ->
            case internalHelp cmd of
                Just msg ->
                    message defaultLayoutOptions verbosity colour stdout msg

                Nothing ->
                    External.executeCommand appNames cmd ["--help"] config

        ManPage _topic _config ->
            -- TODO: Implement this.
            pure ()
  where
    defaults = Mainplate.applySimpleDefaults (MainHelp ())

    Config{colourOutput, extraHelpMessage, verbosity} = config
    colour = fromMaybe ColourOutput.Auto colourOutput

-- TODO:
--
-- > TOOLSET [GLOBAL_OPTIONS] help --man TOPIC
parseOptions :: AppNames -> Config -> [String] -> IO (Endo (HelpMode ()))
parseOptions appNames config@Config{aliases} options =
    execParser $ foldEndo
        <$> optional
                ( helpFlag
                <|> manFlag
                <|> subcommandArg
                )
  where
    switchTo = Endo . const

    manFlag, helpFlag, subcommandArg :: Options.Parser (Endo (HelpMode ()))

    manFlag =
        Options.strOption (Options.long "man" <> Options.metavar "TOPIC")
            <&> \topic -> switchTo (ManPage topic ())

    helpFlag =
        Options.flag mempty (switchTo $ SubcommandHelp "help" ())
            (Options.long "help" <> Options.short 'h')

    subcommandArg =
        Options.strArgument (Options.metavar "SUBCOMMAND") <&> \cmd ->
            let (realCmd, _) = applyAlias aliases cmd []
            in switchTo (SubcommandHelp realCmd ())

    execParser parser =
        Options.internalSubcommandParse appNames config "help"
            Options.defaultPrefs (Options.info parser mempty) options

mainHelpMsg :: AppNames -> Pretty.Doc (Result Pretty.AnsiStyle)
mainHelpMsg AppNames{usedName} = Pretty.vsep
    [ usageSection usedName
        [ subcommand <+> subcommandArguments
        , "help" <+> optionalMetavar "HELP_OPTIONS" <+> optionalSubcommand
        , "config" <+> optionalMetavar "CONFIG_OPTIONS" <+> optionalSubcommand
        , "version" <+> optionalMetavar "VERSION_OPTIONS"
        , "completion" <+> optionalMetavar "COMPLETION_OPTIONS"
        , Pretty.braces (longOption "version" <> "|" <> shortOption 'V')
        , helpOptions
        ]

    , section (Pretty.annotate dullGreen "GLOBAL_OPTIONS" <> ":")
        [ optionDescription ["-v"]
            [ Pretty.reflow "Increment verbosity by one level. Can be used"
            , Pretty.reflow "multiple times."
            ]

        , optionDescription ["--verbosity=VERBOSITY"]
            [ Pretty.reflow "Set verbosity level to"
            , metavar "VERBOSITY" <> "."
            , Pretty.reflow "Possible values of"
            , metavar "VERBOSITY", "are"
            , Pretty.squotes (value "silent") <> ","
            , Pretty.squotes (value "normal") <> ","
            , Pretty.squotes (value "verbose") <> ","
            , "and"
            , Pretty.squotes (value "annoying") <> "."
            ]

        , optionDescription ["--silent", "-s"]
            (silentDescription "quiet")

        , optionDescription ["--quiet", "-q"]
            (silentDescription "silent")

        , optionDescription ["--colo[u]r=WHEN"]
            [ "Set", metavar "WHEN"
            , Pretty.reflow "colourised output should be produced. Possible"
            , Pretty.reflow "values of"
            , metavar "WHEN", "are", Pretty.squotes (value "always") <> ","
            , Pretty.squotes (value "auto") <> ","
            , "and", Pretty.squotes (value "never") <> "."
            ]

        , optionDescription ["--no-colo[u]r"]
            [ Pretty.reflow "Same as"
            , Pretty.squotes (longOption "colour=never") <> "."
            ]

        , optionDescription ["--no-aliases"]
            [ "Ignore", metavar "SUBCOMMAND", Pretty.reflow "aliases. This is\
                \ useful when used from e.g. scripts to avoid issues with user\
                \ defined aliases interfering with how the script behaves."
            ]

        , optionDescription ["--version", "-V"]
            [ Pretty.reflow "Print version information to stdout and exit."
            ]

        , optionDescription ["--help", "-h"]
            [ Pretty.reflow "Print this help and exit."
            ]
        ]

    , section (metavar "SUBCOMMAND" <> ":")
        [ Pretty.reflow "Name of either internal or external subcommand."
        ]
    , ""
    ]
  where
    silentDescription altOpt =
        [ Pretty.reflow
            "Silent mode. Suppress normal diagnostic or result output. Same as"
        , Pretty.squotes (longOption altOpt) <> ",", "and"
        , Pretty.squotes (longOption "verbosity=silent") <> "."
        ]

helpSubcommandHelp :: AppNames -> Pretty.Doc (Result Pretty.AnsiStyle)
helpSubcommandHelp AppNames{usedName} = Pretty.vsep
    [ usageSection usedName
        [ "help" <+> subcommand
--      , "help" <+> longOption "man" <> "=" <> metavar "TOPIC"
        , "help" <+> helpOptions
        , helpOptions
        ]

    , section "Options:"
        [ optionDescription ["SUBCOMMAND"]
            [ Pretty.reflow "Name of an internal or external subcommand for"
            , Pretty.reflow "which to show help message."
            ]

--      , optionDescription ["--man=TOPIC"]
--          [ Pretty.reflow "Show manual page for"
--          , metavar "TOPIC"
--          , Pretty.reflow "instead of short help message."
--          ]

        , optionDescription ["--help", "-h"]
            [ Pretty.reflow "Print this help and exit. Same as"
            , Pretty.squotes (toolsetCommand usedName "help help") <> "."
            ]

        , globalOptionsHelp usedName
        ]

    , ""
    ]

helpOptions :: Pretty.Doc (Result Pretty.AnsiStyle)
helpOptions = Pretty.braces (longOption "help" <> "|" <> shortOption 'h')

subcommand :: Pretty.Doc (Result Pretty.AnsiStyle)
subcommand = metavar "SUBCOMMAND"

optionalSubcommand :: Pretty.Doc (Result Pretty.AnsiStyle)
optionalSubcommand = Pretty.brackets subcommand

subcommandArguments :: Pretty.Doc (Result Pretty.AnsiStyle)
subcommandArguments =
    Pretty.brackets (Pretty.annotate dullGreen "SUBCOMMAND_ARGUMENTS")

globalOptions :: Pretty.Doc (Result Pretty.AnsiStyle)
globalOptions = Pretty.brackets (Pretty.annotate dullGreen "GLOBAL_OPTIONS")

dullGreen, magenta :: Result Pretty.AnsiStyle
dullGreen = Result (Pretty.colorDull Pretty.Green)
magenta = Result (Pretty.color Pretty.Magenta)

usageSection
    :: Pretty command
    => command
    -> [Pretty.Doc (Result Pretty.AnsiStyle)]
    -> Pretty.Doc (Result Pretty.AnsiStyle)
usageSection commandName ds =
    section "Usage:"
        $   ( ds <&> \rest ->
                pretty commandName <+> globalOptions <+> rest
            )
        <>  [""]

globalOptionsHelp
    :: Pretty toolset
    => toolset
    -> Pretty.Doc (Result Pretty.AnsiStyle)
globalOptionsHelp toolset =
    optionDescription ["GLOBAL_OPTIONS"]
        [ Pretty.reflow "See output of" <++> Pretty.squotes callHelp <> "."
        ]
  where
    callHelp = toolsetCommand toolset "help"

section :: Pretty.Doc ann -> [Pretty.Doc ann] -> Pretty.Doc ann
section d ds = Pretty.nest 2 $ Pretty.vsep (d : "" : ds)

optionDescription
    :: [Text]
    -> [Pretty.Doc (Result Pretty.AnsiStyle)]
    -> Pretty.Doc (Result Pretty.AnsiStyle)
optionDescription opts ds = Pretty.vsep
    [ prettyOpts opts
    , Pretty.indent 4 $ Pretty.fillSep ds
    , ""
    ]
  where
    prettyOpts =
        Pretty.hsep
        . Pretty.punctuate Pretty.comma
        . fmap (Pretty.annotate dullGreen . pretty)

shortOption :: Char -> Pretty.Doc (Result Pretty.AnsiStyle)
shortOption o = Pretty.annotate dullGreen ("-" <> pretty o)

longOption :: Text -> Pretty.Doc (Result Pretty.AnsiStyle)
longOption o = Pretty.annotate dullGreen ("--" <> pretty o)

metavar :: Text -> Pretty.Doc (Result Pretty.AnsiStyle)
metavar = Pretty.annotate dullGreen . pretty

value :: Text -> Pretty.Doc (Result Pretty.AnsiStyle)
value = Pretty.annotate dullGreen . pretty

optionalMetavar :: Text -> Pretty.Doc (Result Pretty.AnsiStyle)
optionalMetavar = Pretty.brackets . metavar

command
    :: Pretty.Doc (Result Pretty.AnsiStyle)
    -> Pretty.Doc (Result Pretty.AnsiStyle)
command = Pretty.annotate magenta

toolsetCommand
    :: Pretty toolset
    => toolset
    -> Pretty.Doc (Result Pretty.AnsiStyle)
    -> Pretty.Doc (Result Pretty.AnsiStyle)
toolsetCommand toolset doc = Pretty.annotate magenta (pretty toolset <+> doc)

(<++>) :: Pretty.Doc ann -> Pretty.Doc ann -> Pretty.Doc ann
x <++> y = x <> Pretty.softline <> y
