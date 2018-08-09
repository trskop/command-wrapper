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
    ( Command(..)
    , run

    , runMain

    , HelpMode(..)
    , help

    , ConfigMode(..)
    , config
    )
  where

import Prelude (error)

import Control.Applicative (pure)
import Data.Foldable (traverse_)
import Data.Function (($), (.), const)
import Data.Functor (Functor)
import Data.Monoid (Endo(Endo), mempty)
import Data.Semigroup ((<>))
import Data.String (String, unlines)
import System.IO (IO, putStr, putStrLn)

import qualified Mainplate (applySimpleDefaults, noConfigToRead, runAppWith)

import qualified CommandWrapper.Config as Global (Config(..))
import qualified CommandWrapper.External as External (executeCommand)


data Command
    = HelpCmommand [String]
    | ConfigCommand [String]

run :: String -> Command -> Global.Config -> IO ()
run appName = \case
    HelpCmommand options -> help appName options
    ConfigCommand options -> config appName options

runMain
    :: Functor mode
    => IO (Endo (mode config))
    -> (Endo (mode config) -> IO (mode config))
    -> (mode config -> IO ())
    -> IO ()
runMain parseOptions =
    Mainplate.runAppWith parseOptions (pure . Mainplate.noConfigToRead)

-- {{{ Help Command -----------------------------------------------------------

-- {{{ Help Command -----------------------------------------------------------

data HelpMode a
    = MainHelp a
    | SubcommandHelp String a
  deriving Functor

help :: String -> [String] -> Global.Config -> IO ()
help appName options globalConfig =
    runMain parseOptions defaults $ \case
        MainHelp Global.Config{Global.extraHelpMessage} -> do
            putStr helpMsg
            traverse_ putStrLn extraHelpMessage

        SubcommandHelp subcommand cfg ->
            -- TODO: Resolve possible aliases for subcommand.
            External.executeCommand (appName <> "-") subcommand ["--help"] cfg
  where
    defaults = Mainplate.applySimpleDefaults (MainHelp globalConfig)

    parseOptions :: IO (Endo (HelpMode Global.Config))
    parseOptions = case options of
        [] -> switchTo (MainHelp globalConfig)
        [subcommand] -> switchTo (SubcommandHelp subcommand globalConfig)
        _ -> error "Too many arguments"

    switchTo = pure . Endo . const

    helpMsg = unlines
        [ "Usage:"
        , ""
        , "  " <> appName <> " [GLOBAL_OPTIONS] SUBCOMMAND [SUBCOMMAND_ARGUMENTS]"
        , "  " <> appName <> " help [SUBCOMMAND]"
        , "  " <> appName <> " {-h|--help}"
        ]

-- }}} Help Command -----------------------------------------------------------

-- {{{ Config Command ---------------------------------------------------------

newtype ConfigMode a
    = InitConfig a
  deriving Functor

config :: String -> [String] -> Global.Config -> IO ()
config _appName _options globalConfig =
    runMain parseOptions defaults $ \case
        InitConfig _ -> pure ()
  where
    defaults = Mainplate.applySimpleDefaults (InitConfig globalConfig)

    parseOptions :: IO (Endo (ConfigMode Global.Config))
    parseOptions = pure mempty

-- }}} Config Command ---------------------------------------------------------
