{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
-- |
-- Module:      $Header$
-- Description: Internal commands supported by CommandWrapper
-- Copyright:   (c) 2018-2020 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Internal commands supported by CommandWrapper.
module CommandWrapper.Toolset.InternalSubcommand
    (
    -- * Internal Commands
      Command(..)
    , run
    , command

    -- ** Help Command
    , Subcommand.HelpOptions(..)
    , Subcommand.SubcommandDescription(..)
    , Subcommand.help
    , Subcommand.helpSubcommandCompleter
    , Subcommand.helpSubcommandDescription
    , Subcommand.helpSubcommandHelp

    -- ** Config Command
    , Subcommand.config
    , Subcommand.configSubcommandCompleter
    , Subcommand.configSubcommandHelp

    -- ** Completion Command
    , Subcommand.CompletionConfig(..)
    , Subcommand.Completer
    , Subcommand.InternalCompleter
    , Subcommand.completion
    , Subcommand.completionSubcommandCompleter
    , Subcommand.completionSubcommandDescription
    , Subcommand.completionSubcommandHelp

    -- ** Version Command
    , Subcommand.VersionInfo(..)
    , Subcommand.PrettyVersion(..)
    , Subcommand.version
    , Subcommand.versionSubcommandCompleter
    , Subcommand.versionSubcommandDescription
    , Subcommand.versionSubcommandHelp

    -- * Generic Functions
    , runMain
    )
  where

import Control.Applicative ((<*>), pure)
import Data.Functor ((<$>), (<&>), fmap)
import qualified Data.List as List (lookup)
import qualified Data.List.NonEmpty as NonEmpty (last, toList)
import Data.Maybe (Maybe(..))
import Data.Semigroup ((<>))
import Data.String (String)
import Data.Tuple (fst, snd)
import Data.Version (makeVersion)
import GHC.Generics (Generic)
import System.IO (IO)
import Text.Show (Show)

import qualified Data.Text.Prettyprint.Doc as Pretty (Doc)
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Pretty (AnsiStyle)

import CommandWrapper.Core.Environment
    ( AppNames(AppNames, names)
    , subcommandProtocolVersion
    )
import CommandWrapper.Core.Message (Result)
import qualified CommandWrapper.Toolset.Config.Global as Global (Config)
import qualified CommandWrapper.Toolset.InternalSubcommand.Completion as Subcommand
    ( Completer
    , CompletionConfig(..)
    , InternalCompleter
    , completion
    , completionSubcommandCompleter
    , completionSubcommandDescription
    , completionSubcommandHelp
    )
import qualified CommandWrapper.Toolset.InternalSubcommand.Config as Subcommand
    ( config
    , configSubcommandCompleter
    , configSubcommandDescription
    , configSubcommandHelp
    )
import qualified CommandWrapper.Toolset.InternalSubcommand.Help as Subcommand
    ( HelpOptions(..)
    , SubcommandDescription(..)
    , help
    , helpSubcommandCompleter
    , helpSubcommandDescription
    , helpSubcommandHelp
    )
import qualified CommandWrapper.Toolset.InternalSubcommand.Version as Subcommand
    ( VersionInfo(..)
    , PrettyVersion(..)
    , version
    , versionQQ
    , versionSubcommandCompleter
    , versionSubcommandDescription
    , versionSubcommandHelp
    )
import CommandWrapper.Toolset.InternalSubcommand.Utils (runMain)

import Paths_command_wrapper (version) -- Temporary


data Command
    = HelpCommand [String]
    | ConfigCommand [String]
    | CompletionCommand [String]
    | VersionCommand [String]
  deriving stock (Generic, Show)

-- | Smart constructor for 'Command'.
command
    :: String
    -- ^ Subcommand name.
    -> [String]
    -- ^ Subcommand arguments.
    -> Maybe Command
    -- ^ Return 'Nothing' if subcommand is not an internal command.
command name args = fmap snd (List.lookup name commands) <*> pure args

commands :: [(String, (String, [String] -> Command))]
commands =
    [   ( "completion"
        , (Subcommand.completionSubcommandDescription, CompletionCommand)
        )
    ,   ( "config"
        , (Subcommand.configSubcommandDescription, ConfigCommand)
        )
    ,   ( "help"
        , (Subcommand.helpSubcommandDescription, HelpCommand)
        )
    ,   ( "version"
        , (Subcommand.versionSubcommandDescription, VersionCommand)
        )
    ]

run :: (AppNames -> Global.Config -> Pretty.Doc (Result Pretty.AnsiStyle))
    -> AppNames
    -> Command
    -> Global.Config
    -> IO ()
run mainHelp appNames@AppNames{names} = \case
    HelpCommand options ->
        Subcommand.help helpOptions appNames options

    ConfigCommand options ->
        Subcommand.config appNames options

    CompletionCommand options ->
        Subcommand.completion completionConfig appNames options

    VersionCommand options ->
        Subcommand.version versionInfo appNames options
  where
    -- TODO: Is this the best place for this definition?
    versionInfo = Subcommand.VersionInfo
        { commandWrapper = Subcommand.PrettyVersion version

        , subcommandProtocol =
            Subcommand.PrettyVersion subcommandProtocolVersion

        , dhallLibrary =
            Subcommand.PrettyVersion [Subcommand.versionQQ|VERSION_dhall|]

        -- This is a hacky way how to print Dhall Standard that Dhall library
        -- is using.  Unfortunately there is no nicer alternative at the
        -- moment.
        , dhallStandard = Subcommand.PrettyVersion (makeVersion [17, 0, 0])
        }

    completionConfig = Subcommand.CompletionConfig
        { internalCompleter = \subcommandName ->
            internalSubcommandCompleter internalSubcommandNames helpOptions
                <$> command subcommandName []

        , internalSubcommands = internalSubcommandNames
        }

    helpOptions = Subcommand.HelpOptions
        { internalHelp = \s ->
            internalSubcommandHelp <$> command s []
        , mainHelp
        , internalSubcommands
        , topics = fst <$> topicsToPages
        , toManualPage = \s ->
            List.lookup s
                (topicsToPages <> fmap stdTopic internalSubcommandNames)
        }

    internalSubcommandNames = fst <$> commands

    internalSubcommands = commands <&> \(name, (description, _)) ->
        Subcommand.SubcommandDescription{name, description}

    -- Original command wrapper executable name.  This will most certainly be
    -- `command-wrapper`.
    stdPrefix = NonEmpty.last names

    -- Most topic manual pages follow this convention:
    stdTopic s = (s, stdPrefix <> "-" <> s)

    topicsToPages =
        -- TODO: At some point we would like to involve lookup of manual pages
        -- instead of hardcoded list.  Not having it hardcoded inside `help`
        -- subcommand is just the first step there.
        [ stdTopic "bash-library"
        , stdTopic "default.dhall"
        , stdTopic "direnv-library"
        , stdTopic "subcommand-protocol"
        ]
        -- These are toolset names. We want to allow them to be called
        -- explicitly.
        <> (NonEmpty.toList names <&> \s -> (s, s))

internalSubcommandCompleter
    :: [String]
    -> Subcommand.HelpOptions
    -> Command
    -> Subcommand.Completer
internalSubcommandCompleter internalSubcommands helpOptions = \case
    HelpCommand _ ->
        Subcommand.helpSubcommandCompleter helpOptions

    ConfigCommand _ ->
        Subcommand.configSubcommandCompleter

    CompletionCommand _ ->
        Subcommand.completionSubcommandCompleter internalSubcommands

    VersionCommand _ ->
        Subcommand.versionSubcommandCompleter

internalSubcommandHelp
    :: Command
    -> AppNames
    -> Global.Config
    -> Pretty.Doc (Result Pretty.AnsiStyle)
internalSubcommandHelp = \case
    HelpCommand _ ->
        Subcommand.helpSubcommandHelp

    ConfigCommand _ ->
        Subcommand.configSubcommandHelp

    CompletionCommand _ ->
        Subcommand.completionSubcommandHelp

    VersionCommand _ ->
        Subcommand.versionSubcommandHelp
