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
    , ConfigMode(..)
    , config

    -- ** Completion Command
    , Subcommand.CompletionMode(..)
    , Subcommand.completion

    -- * Generic Functions
    , runMain
    )
  where

import Control.Applicative (pure)
import Data.Function (($), (.), const)
import Data.Functor (Functor, (<$>))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (Endo)
import Data.String (String, fromString)
import GHC.Generics (Generic)
import System.IO (IO, stderr)
import Text.Show (Show)

import qualified Data.Text.Prettyprint.Doc as Pretty (Doc)
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Pretty (AnsiStyle)
import qualified Mainplate (applySimpleDefaults)

import qualified CommandWrapper.Config.Global as Global (Config(..))
import CommandWrapper.Environment (AppNames(AppNames, usedName))
import qualified CommandWrapper.Internal.Subcommand.Completion as Subcommand
    ( CompletionMode(..)
    , completion
    , completionSubcommandHelp
    )
import qualified CommandWrapper.Internal.Subcommand.Help as Subcommand
    ( HelpMode(..)
    , help
    , helpSubcommandHelp
    )
import CommandWrapper.Internal.Utils (runMain)
import CommandWrapper.Message (Result, dieSubcommandNotYetImplemented)
import qualified CommandWrapper.Options.ColourOutput as ColourOutput
    ( ColourOutput(Auto)
    )


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

    ConfigCommand _ -> "Subcommand currently not implemented."

    CompletionCommand _ ->
        Subcommand.completionSubcommandHelp appNames


-- }}} Help Command -----------------------------------------------------------

-- {{{ Config Command ---------------------------------------------------------

data ConfigMode a
    = InitConfig a
    | ConfigLib a
    | Dhall a
  deriving (Functor, Generic, Show)

config :: AppNames -> [String] -> Global.Config -> IO ()
config AppNames{usedName} _options globalConfig =
    runMain parseOptions defaults $ \case
        InitConfig _ -> pure ()
        ConfigLib _ -> pure ()

        -- TODO:
        --
        -- - Merge in functionality of: `dhall`, `dhall-json`, `dhall-bash`,
        --   and `dhall-text`
        -- - Provide functionality for shell variables that transforms:
        --
        --     ```
        --     { name = "FOO"
        --     , value = "foo"
        --     }
        --     ```
        --
        --     Into:
        --
        --     ```
        --     export FOO=foo
        --     ```
        --
        --     It should also support transorming:
        --
        --     ```
        --     [ { name = "FOO"
        --       , value = "foo"
        --       }
        --     , { name = "BAR"
        --       , value = "bar"
        --       }
        --     ]
        --     ```
        --
        --     Into:
        --
        --     ```
        --     export FOO=foo
        --     export FOO=bar
        --     ```
        Dhall _ -> pure ()
  where
    defaults = Mainplate.applySimpleDefaults (InitConfig globalConfig)

    parseOptions :: IO (Endo (ConfigMode Global.Config))
    parseOptions =
        let Global.Config
                { Global.verbosity
                , Global.colourOutput = possiblyColourOutput
                } = globalConfig

        in dieSubcommandNotYetImplemented (fromString usedName) verbosity
            (fromMaybe ColourOutput.Auto possiblyColourOutput) stderr "config"

-- }}} Config Command ---------------------------------------------------------
