{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
-- |
-- Module:      CommandWrapper.Internal
-- Description: Internal commands supported by CommandWrapper
-- Copyright:   (c) 2018 Peter Trško
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
    , HelpMode(..)
    , help

    -- ** Config Command
    , ConfigMode(..)
    , config

    -- ** Completion Command
    , CompletionMode(..)
    , completion

    -- * Generic Functions
    , runMain
    )
  where

import Control.Applicative (pure)
import Data.Foldable (mapM_, traverse_)
import Data.Function (($), (.), const)
import Data.Functor (Functor, (<$>))
import qualified Data.List as List (nub)
import Data.Maybe (Maybe(..), isJust)
import Data.Monoid (Endo(Endo))
import Data.Semigroup ((<>))
import Data.String (String, unlines)
import GHC.Generics (Generic)
import System.Exit (die)
import System.IO (IO, putStr, putStrLn)
import Text.Show (Show)

import qualified Mainplate (applySimpleDefaults, noConfigToRead, runAppWith)

import qualified CommandWrapper.Config as Global (Config(..))
import CommandWrapper.Environment (AppNames(AppNames, usedName))
import qualified CommandWrapper.External as External
    ( executeCommand
    , findSubcommands
    )
import CommandWrapper.Options.Alias (Alias(alias), applyAlias)


data Command
    = HelpCmommand [String]
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
    "help" -> Just . HelpCmommand
    "config" -> Just . ConfigCommand
    "completion" -> Just . CompletionCommand
    _ -> const Nothing

run :: AppNames -> Command -> Global.Config -> IO ()
run appNames = \case
    HelpCmommand options -> help appNames options
    ConfigCommand options -> config appNames options
    CompletionCommand options -> completion appNames options

-- {{{ Help Command -----------------------------------------------------------

-- {{{ Help Command -----------------------------------------------------------

data HelpMode a
    = MainHelp a
    | SubcommandHelp String a
  deriving (Functor, Generic, Show)

help :: AppNames -> [String] -> Global.Config -> IO ()
help appNames@AppNames{usedName} options globalConfig =
    runMain parseOptions defaults $ \case
        MainHelp Global.Config{Global.extraHelpMessage} -> do
            putStr helpMsg
            traverse_ putStrLn extraHelpMessage

        SubcommandHelp subcommand cfg ->
            External.executeCommand appNames subcommand ["--help"] cfg
  where
    defaults = Mainplate.applySimpleDefaults (MainHelp globalConfig)

    parseOptions :: IO (Endo (HelpMode Global.Config))
    parseOptions = case options of
        [] ->
            switchTo (MainHelp globalConfig)

        [subcmd] ->
            let (subcmd', _) =
                    applyAlias (Global.aliases globalConfig) subcmd []
            in switchTo
                $ if isJust (command subcmd' [])
                    then
                        -- TODO: We should have more detailed help for internal
                        -- commands.
                        MainHelp globalConfig
                    else
                        SubcommandHelp subcmd' globalConfig

        _ : arg : _ ->
            die ("Error: '" <> arg <> "': Too many arguments")

    switchTo = pure . Endo . const

    helpMsg = unlines
        [ "Usage:"
        , ""
        , "  " <> usedName <> " [GLOBAL_OPTIONS] SUBCOMMAND [SUBCOMMAND_ARGUMENTS]"
        , "  " <> usedName <> " config [SUBCOMMAND]"
        , "  " <> usedName <> " help [SUBCOMMAND]"
        , "  " <> usedName <> " {-h|--help}"
        ]

-- }}} Help Command -----------------------------------------------------------

-- {{{ Config Command ---------------------------------------------------------

data ConfigMode a
    = InitConfig a
    | ConfigLib a
    | Dhall a
  deriving (Functor, Generic, Show)

config :: AppNames -> [String] -> Global.Config -> IO ()
config _appNames _options globalConfig =
    runMain parseOptions defaults $ \case
        InitConfig _ -> pure ()
        ConfigLib _ -> pure ()

        -- TODO:
        --
        -- * Merge in functionality of: `dhall`, `dhall-json`, `dhall-bash`,
        --   and `dhall-text`
        -- * Provide functionality for shell variables that transforms:
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
    parseOptions = die "Error: config: Subcommand not yet implemented."

-- }}} Config Command ---------------------------------------------------------

-- {{{ Completion Command -----------------------------------------------------

data Shell = Bash
  deriving (Generic, Show)

data WhatArgumentsToList = Subcommands
  deriving (Generic, Show)
    -- TODO: Extend this data type to be able to list various combinations of
    -- following groups:
    --
    -- * External subcommands
    -- * Internal subcommands
    -- * Aliases
    -- * Global options

data CompletionMode a
    = CompletionMode a
    | GenerateCompletionScriptMode Shell a
    | ListArgumentsMode WhatArgumentsToList a
  deriving (Functor, Generic, Show)

completion :: AppNames -> [String] -> Global.Config -> IO ()
completion appNames _options globalConfig =
    runMain parseOptions defaults $ \case
        -- TODO:
        --
        -- * This subcommand will need access to global parser definition to
        --   provide completion for global options.
        --
        -- * When completing subcommand names we need to find possible
        --   completions in:
        --
        --     - All possible external and internal subcommands.
        --     - All aliases defined by `command-wrapper` and `TOOLSET` (if not
        --       invoking `command-wrapper` directly).
        --
        -- * When completing option/argument of a subcommand we need to execute
        --   the subcommand with bash completion options passed to it. This
        --   will require us to extend `SUBCOMMAND_PROTOCOL.md`. Should we rely
        --   on `optparse-applicative` for this?
        CompletionMode _config -> pure ()

        -- TODO:
        --
        -- * Completion script should be configurable.
        --
        -- * By default it should be printed to `stdout`, but we should support
        --   writting it into a file without needing to redirect `stdout`.
        GenerateCompletionScriptMode _shell _config -> pure ()

        ListArgumentsMode whatToList cfg -> case whatToList of
            Subcommands -> do
                let aliases = alias <$> Global.aliases cfg
                    internalCommands = ["help", "config", "completion"]
                cmds <- External.findSubcommands appNames cfg
                mapM_ putStrLn (List.nub $ aliases <> internalCommands <> cmds)
  where
    defaults = Mainplate.applySimpleDefaults (CompletionMode globalConfig)

    parseOptions :: IO (Endo (CompletionMode Global.Config))
    parseOptions =
        -- TODO: Temporary hack to make this subcommand somewhat useful.
        pure . Endo $ const (ListArgumentsMode Subcommands globalConfig)

-- }}} Completion Command -----------------------------------------------------

-- {{{ Generic Functions ------------------------------------------------------

-- | Simplified version of 'Mainplate.runAppWith' that assumes that we don't
-- need to parse any additional configuration file. This should be true for
-- internal commands.
runMain
    :: Functor mode
    => IO (Endo (mode config))
    -> (Endo (mode config) -> IO (mode config))
    -> (mode config -> IO ())
    -> IO ()
runMain parseOptions =
    Mainplate.runAppWith parseOptions (pure . Mainplate.noConfigToRead)

-- }}} ------------------------------------------------------------------------
