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
    )
  where

import Data.Foldable (traverse_)
import Data.Maybe (fromMaybe)
import Data.Monoid (Endo(Endo))
import Data.String (fromString)
import GHC.Generics (Generic)
import System.IO (stderr)

import qualified Mainplate (applySimpleDefaults)
import qualified Data.Output.Colour as ColourOutput (ColourOutput(Auto))

import CommandWrapper.Config.Global (Config(..))
import CommandWrapper.Environment (AppNames(AppNames, usedName))
import qualified CommandWrapper.External as External (executeCommand)
import CommandWrapper.Internal.Utils (runMain)
import qualified CommandWrapper.Message as Message (dieTooManyArguments)
import CommandWrapper.Options.Alias (applyAlias)


data HelpMode a
    = MainHelp a
    | SubcommandHelp String a
    | ManPage String a
  deriving stock (Functor, Generic, Show)

help
    :: (String -> Maybe String)
    -- ^ Return help message to print if string argument is an internal
    -- subcommand.
    --
    -- TODO: Return something more structured.
    -> AppNames
    -> [String]
    -> Config
    -> IO ()
help internalHelp appNames options config@Config{extraHelpMessage} =
    runMain (parseOptions appNames config options) defaults $ \case
        MainHelp _config -> do
            putStr (mainHelpMsg appNames)
            traverse_ putStrLn extraHelpMessage

        SubcommandHelp cmd _config ->
            case internalHelp cmd of
                Just msg ->
                    putStr msg

                Nothing ->
                    External.executeCommand appNames cmd ["--help"] config

        ManPage _topic _config ->
            pure ()
  where
    defaults = Mainplate.applySimpleDefaults (MainHelp ())

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

mainHelpMsg :: AppNames -> String
mainHelpMsg AppNames{usedName} = unlines
    [ "Usage:"
    , ""
    , "  " <> usedName <> " [GLOBAL_OPTIONS] SUBCOMMAND [SUBCOMMAND_ARGUMENTS]"
    , "  " <> usedName <> " config [SUBCOMMAND]"
    , "  " <> usedName <> " help [SUBCOMMAND]"
    , "  " <> usedName <> " {-h|--help}"
    , ""
    , "Global options:"
    , ""
    , "  -v"
    , "     Increment verbosity by one level. Can be used multiple times."
    , ""

    , "  --verbosity=VERBOSITY"
    , "     Set verbosity level to VERBOSITY. Possible values of VERBOSITY are\
        \ 'silent', 'normal', 'verbose', and 'annoying'."
    , ""
    , "  --silent, -s"
    , "    Silent mode. Suppress normal diagnostic or result output. Same as\
        \ '--quiet' and '--verbosity=silent'."
    , ""
    , "  --quiet, -q"
    , "    Silent mode. Suppress normal diagnostic or result output. Same as\
        \ '--silent' and '--verbosity=silent'."
    , ""
    , "  --colo[u]r=WHEN"
    , "    Set WHEN colourised output should be produced. Possible values of\
        \ WHEN are 'always', 'auto', and 'never'."
    , ""
    , "  --no-colo[u]r"
    , "    Same as '--colour=no'."
    ]

helpSubcommandHelp :: AppNames -> String
helpSubcommandHelp AppNames{usedName} = unlines
    [ "Usage:"
    , ""
    , "  " <> usedName <> " [GLOBAL_OPTIONS] help [SUBCOMMAND]"
    , "  " <> usedName <> " [GLOBAL_OPTIONS] {--help|-h}"
    , ""
    , "Options:"
    , ""
    , "  SUBCOMMAND"
    , "    Name of a subcommand for which to show help message."
    , ""
    , "Global options:"
    , ""
    , "  See output of '" <> usedName <> " help'."
    ]

dieTooManyArguments :: AppNames -> Config -> String -> IO a
dieTooManyArguments AppNames{usedName} Config{verbosity, colourOutput} arg =
    Message.dieTooManyArguments (fromString usedName) "help" verbosity
        (fromMaybe ColourOutput.Auto colourOutput) stderr (fromString arg)
