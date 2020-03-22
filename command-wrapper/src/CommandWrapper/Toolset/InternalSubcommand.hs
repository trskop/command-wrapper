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
    , Subcommand.help
    , Subcommand.helpSubcommandCompleter
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
    , Subcommand.completionSubcommandHelp

    -- ** Version Command
    , Subcommand.VersionInfo(..)
    , Subcommand.PrettyVersion(..)
    , Subcommand.version
    , Subcommand.versionSubcommandCompleter
    , Subcommand.versionSubcommandHelp

    -- * Generic Functions
    , runMain
    )
  where

import Control.Applicative ((<*>), pure)
import Data.Functor ((<$>))
import qualified Data.List as List (lookup)
import Data.Maybe (Maybe(..))
import Data.String (String)
import Data.Tuple (fst)
import Data.Version (makeVersion)
import GHC.Generics (Generic)
import System.IO (IO)
import Text.Show (Show)

import qualified Data.Text.Prettyprint.Doc as Pretty (Doc)
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Pretty (AnsiStyle)

import CommandWrapper.Core.Environment (AppNames, subcommandProtocolVersion)
import CommandWrapper.Core.Message (Result)
import qualified CommandWrapper.Toolset.Config.Global as Global (Config)
import qualified CommandWrapper.Toolset.InternalSubcommand.Completion as Subcommand
    ( Completer
    , CompletionConfig(..)
    , InternalCompleter
    , completion
    , completionSubcommandCompleter
    , completionSubcommandHelp
    )
import qualified CommandWrapper.Toolset.InternalSubcommand.Config as Subcommand
    ( config
    , configSubcommandCompleter
    , configSubcommandHelp
    )
import qualified CommandWrapper.Toolset.InternalSubcommand.Help as Subcommand
    ( help
    , helpSubcommandCompleter
    , helpSubcommandHelp
    )
import qualified CommandWrapper.Toolset.InternalSubcommand.Version as Subcommand
    ( VersionInfo(..)
    , PrettyVersion(..)
    , version
    , versionQQ
    , versionSubcommandCompleter
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
command name args = List.lookup name commands <*> pure args

commands :: [(String, [String] -> Command)]
commands =
    [ ("completion", CompletionCommand)
    , ("config", ConfigCommand)
    , ("help", HelpCommand)
    , ("version", VersionCommand)
    ]

run :: (AppNames -> Global.Config -> Pretty.Doc (Result Pretty.AnsiStyle))
    -> AppNames
    -> Command
    -> Global.Config
    -> IO ()
run globalHelp appNames = \case
    HelpCommand options ->
        help globalHelp appNames options

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
        , dhallStandard = Subcommand.PrettyVersion (makeVersion [14, 0, 0])
        }

    completionConfig = Subcommand.CompletionConfig
        { internalCompleter = \subcommandName ->
            internalSubcommandCompleter <$> command subcommandName []

        , internalSubcommands = fst <$> commands
        }

internalSubcommandCompleter :: Command -> Subcommand.Completer
internalSubcommandCompleter = \case
    HelpCommand _ ->
        Subcommand.helpSubcommandCompleter

    ConfigCommand _ ->
        Subcommand.configSubcommandCompleter

    CompletionCommand _ ->
        Subcommand.completionSubcommandCompleter internalSubcommands

    VersionCommand _ ->
        Subcommand.versionSubcommandCompleter
  where
    internalSubcommands = fst <$> commands

-- {{{ Help Command -----------------------------------------------------------

help
    :: (AppNames -> Global.Config -> Pretty.Doc (Result Pretty.AnsiStyle))
    -> AppNames
    -> [String]
    -> Global.Config
    -> IO ()
help globalHelp = Subcommand.help internalSubcommandHelp' globalHelp
  where
    internalSubcommandHelp' s = internalSubcommandHelp <$> command s []

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

-- }}} Help Command -----------------------------------------------------------
