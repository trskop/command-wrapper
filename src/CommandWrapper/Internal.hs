{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
-- |
-- Module:      CommandWrapper.Internal
-- Description: TODO: Module synopsis
-- Copyright:   (c) 2018 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- TODO: Module description.
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

    -- * Generic Functions
    , runMain
    )
  where

import Control.Applicative (pure)
import Data.Foldable (traverse_)
import Data.Function (($), (.), const)
import Data.Functor (Functor)
import Data.Maybe (Maybe(..), isJust)
import Data.Monoid (Endo(Endo))
import Data.Semigroup ((<>))
import Data.String (String, unlines)
import System.Exit (die)
import System.IO (IO, putStr, putStrLn)

import qualified Mainplate (applySimpleDefaults, noConfigToRead, runAppWith)

import qualified CommandWrapper.Config as Global (Config(..))
import CommandWrapper.Environment (AppNames(AppNames, usedName))
import qualified CommandWrapper.External as External (executeCommand)
import CommandWrapper.Options.Alias (applyAlias)


data Command
    = HelpCmommand [String]
    | ConfigCommand [String]

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
    _ -> const Nothing

run :: AppNames -> Command -> Global.Config -> IO ()
run appNames = \case
    HelpCmommand options -> help appNames options
    ConfigCommand options -> config appNames options

-- {{{ Help Command -----------------------------------------------------------

-- {{{ Help Command -----------------------------------------------------------

data HelpMode a
    = MainHelp a
    | SubcommandHelp String a
  deriving Functor

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

newtype ConfigMode a
    = InitConfig a
  deriving Functor

config :: AppNames -> [String] -> Global.Config -> IO ()
config _appNames _options globalConfig =
    runMain parseOptions defaults $ \case
        InitConfig _ -> pure ()
  where
    defaults = Mainplate.applySimpleDefaults (InitConfig globalConfig)

    parseOptions :: IO (Endo (ConfigMode Global.Config))
    parseOptions = die "Error: config: Subcommand not yet implemented."

-- }}} Config Command ---------------------------------------------------------

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
