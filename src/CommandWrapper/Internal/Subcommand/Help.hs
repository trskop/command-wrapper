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
    , section
    , option
    , globalOptionsSection
    , usageSection
    )
  where

import Data.Foldable (traverse_)
import Data.Maybe (fromMaybe)
import Data.Monoid (Endo(Endo))
import Data.String (fromString)
import GHC.Generics (Generic)
import System.IO (stderr, stdout)

import Data.Text (Text)
import qualified Data.Output.Colour as ColourOutput (ColourOutput(Auto))
import Data.Text.Prettyprint.Doc (Pretty(pretty), (<+>))
import qualified Data.Text.Prettyprint.Doc as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Pretty (AnsiStyle)
import qualified Data.Text.Prettyprint.Doc.Util as Pretty (reflow)
import qualified Mainplate (applySimpleDefaults)

import CommandWrapper.Config.Global (Config(..))
import CommandWrapper.Environment (AppNames(AppNames, usedName))
import qualified CommandWrapper.External as External (executeCommand)
import CommandWrapper.Internal.Utils (runMain)
import CommandWrapper.Message (Result, defaultLayoutOptions, message)
import qualified CommandWrapper.Message as Message (dieTooManyArguments)
import CommandWrapper.Options.Alias (applyAlias)


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
            pure ()
  where
    defaults = Mainplate.applySimpleDefaults (MainHelp ())

    Config{colourOutput, extraHelpMessage, verbosity} = config
    colour = fromMaybe ColourOutput.Auto colourOutput

-- TODO:
--
-- > TOOLSET [GLOBAL_OPTIONS] help --man TOPIC
parseOptions :: AppNames -> Config -> [String] -> IO (Endo (HelpMode ()))
parseOptions appNames config@Config{aliases} = \case
    [] ->
        switchTo (MainHelp ())

    [subcmd] ->
        let (subcmd', _) = applyAlias aliases subcmd []
        in switchTo (SubcommandHelp subcmd' ())

    _ : arg : _ ->
        dieTooManyArguments appNames config arg
  where
    switchTo = pure . Endo . const

mainHelpMsg :: AppNames -> Pretty.Doc (Result Pretty.AnsiStyle)
mainHelpMsg AppNames{usedName} = Pretty.vsep
    [ usageSection usedName
        [ "[GLOBAL_OPTIONS] SUBCOMMAND [SUBCOMMAND_ARGUMENTS]"
        , "config [SUBCOMMAND]"
        , "help [SUBCOMMAND]"
        , "{-h|--help}"
        ]

    , section "Global options:"
        [ option "-v"
            [ "Increment verbosity by one level. Can be used multiple times."
            ]

        , option "--verbosity=VERBOSITY"
            [ "Set verbosity level to VERBOSITY. Possible values of"
            , "VERBOSITY are 'silent', 'normal', 'verbose', and 'annoying'."
            ]

        , option "--silent, -s"
            [ "Silent mode. Suppress normal diagnostic or result output."
            , "Same as '--quiet' and '--verbosity=silent'."
            ]

        , option "--quiet, -q"
            [ "Silent mode. Suppress normal diagnostic or result output."
            , "Same as '--silent' and '--verbosity=silent'."
            ]

        , option "--colo[u]r=WHEN"
            [ "Set WHEN colourised output should be produced."
            , "Possible values of WHEN are 'always', 'auto', and 'never'."
            ]

        , option "--no-colo[u]r"
            [ "Same as '--colour=no'."
            ]
        ]
    ]

helpSubcommandHelp :: AppNames -> Pretty.Doc (Result Pretty.AnsiStyle)
helpSubcommandHelp AppNames{usedName} = Pretty.vsep
    [ usageSection usedName
        [ "[GLOBAL_OPTIONS] help [SUBCOMMAND]"
        , "[GLOBAL_OPTIONS] {--help|-h}"
        ]

    , section "Options:"
        [ option "SUBCOMMAND"
            [ "Name of a subcommand for which to show help message."
            ]
        ]

    , globalOptionsSection usedName
    , ""
    ]

usageSection :: Pretty command => command -> [Pretty.Doc ann] -> Pretty.Doc ann
usageSection commandName ds =
    section "Usage:" $ ((pretty commandName <+>) <$> ds) <> [""]

globalOptionsSection :: Pretty toolset => toolset -> Pretty.Doc ann
globalOptionsSection toolset =
    section "Global options:"
        [ Pretty.reflow "See output of"
            <+> Pretty.squotes (pretty toolset <+> "help") <> "."
        ]

section :: Pretty.Doc ann -> [Pretty.Doc ann] -> Pretty.Doc ann
section d ds = Pretty.nest 2 $ Pretty.vsep (d : "" : ds)

option :: Pretty.Doc ann -> [Text] -> Pretty.Doc ann
option d ds = Pretty.vsep
    [ d
    , Pretty.indent 4 $ Pretty.hsep (Pretty.reflow <$> ds)
    , ""
    ]

dieTooManyArguments :: AppNames -> Config -> String -> IO a
dieTooManyArguments AppNames{usedName} Config{verbosity, colourOutput} arg =
    Message.dieTooManyArguments (fromString usedName) "help" verbosity
        (fromMaybe ColourOutput.Auto colourOutput) stderr (fromString arg)
