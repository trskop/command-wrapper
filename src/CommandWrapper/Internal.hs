{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:      CommandWrapper.Internal
-- Description: Internal commands supported by CommandWrapper
-- Copyright:   (c) 2018-2019 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Internal commands supported by CommandWrapper.
module CommandWrapper.Internal
    (
    -- * Internal Commands
      Command(..)
    , run
    , command

    -- ** Help Command
    , Subcommand.HelpMode(..)
    , help

    -- ** Config Command
    , Subcommand.ConfigMode(..)
    , Subcommand.config

    -- ** Completion Command
    , Subcommand.CompletionMode(..)
    , Subcommand.completion

    -- * Generic Functions
    , runMain
    )
  where

import Data.Function ((.), const)
import Data.Functor ((<$>))
import Data.Maybe (Maybe(..))
import Data.String (String)
import GHC.Generics (Generic)
import System.IO (IO)
import Text.Show (Show)

import qualified Data.Text.Prettyprint.Doc as Pretty (Doc)
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Pretty (AnsiStyle)

import qualified CommandWrapper.Config.Global as Global (Config(..))
import CommandWrapper.Environment (AppNames)
import qualified CommandWrapper.Internal.Subcommand.Completion as Subcommand
    ( CompletionMode(..)
    , completion
    , completionSubcommandHelp
    )
import CommandWrapper.Internal.Subcommand.Config as Subcommand
    ( ConfigMode(..)
    , config
    , configSubcommandHelp
    )
import qualified CommandWrapper.Internal.Subcommand.Help as Subcommand
    ( HelpMode(..)
    , help
    , helpSubcommandHelp
    )
import CommandWrapper.Internal.Utils (runMain)
import CommandWrapper.Message (Result)


data Command
    = HelpCommand [String]
    | ConfigCommand [String]
    | CompletionCommand [String]
  deriving (Generic, Show)

-- | Smart constructor for 'Command'.
command
    :: String
    -- ^ Subcommand name.
    -> [String]
    -- ^ Subcommand arguments.
    -> Maybe Command
    -- ^ Return 'Nothing' if subcommand is not an internal command.
command = \case
    "help" -> Just . HelpCommand
    "config" -> Just . ConfigCommand
    "completion" -> Just . CompletionCommand
    _ -> const Nothing

run :: AppNames -> Command -> Global.Config -> IO ()
run appNames = \case
    HelpCommand options -> help appNames options
    ConfigCommand options -> config appNames options
    CompletionCommand options -> Subcommand.completion appNames options

-- {{{ Help Command -----------------------------------------------------------

help :: AppNames -> [String] -> Global.Config -> IO ()
help appNames = Subcommand.help internalSubcommandHelpMsg appNames
  where
    internalSubcommandHelpMsg s =
        internalSubcommandHelp appNames <$> command s []

internalSubcommandHelp
    :: AppNames
    -> Command
    -> Pretty.Doc (Result Pretty.AnsiStyle)
internalSubcommandHelp appNames = \case
    HelpCommand _ ->
        Subcommand.helpSubcommandHelp appNames

    ConfigCommand _ ->
        Subcommand.configSubcommandHelp appNames

    CompletionCommand _ ->
        Subcommand.completionSubcommandHelp appNames


-- }}} Help Command -----------------------------------------------------------
