{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NamedFieldPuns #-}
-- |
-- Module:       Main
-- Description:  Top-level executable of Command Wrapper.
-- Copyright:    (c) 2014-2018 Peter Trsko
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  GHC specific language extensions.
--
-- Top-level executable of Command Wrapper, it's usually installed as
-- @${HOME}/.local/lib/command-wrapper/command-wrapper@.
module Main (main)
  where

import Control.Applicative ((<*>), many, optional, pure)
import Control.Monad ((>>=))
import Data.Either (Either(Right), either)
import Data.Eq ((/=))
import Data.Function (($), (.), flip, id)
import Data.Functor ((<$>))
import Data.Semigroup ((<>))
import Data.Maybe (Maybe)
import Data.Monoid (Endo(Endo, appEndo), Last(Last, getLast), mconcat, mempty)
import Data.String (String)
import System.Exit (die)
import System.IO (IO{-, print-})
import Text.Show (show)

import qualified Options.Applicative as Options
import qualified Options.Applicative.Standard as Options
import Data.Monoid.Endo.Fold (foldEndo)
import qualified Mainplate.Extensible as Mainplate
    ( Command(Internal)
    , runExtensibleAppWith
    )
import System.Directory
    ( XdgDirectory(XdgConfig)
    , doesFileExist
    , getXdgDirectory
    )
import System.FilePath ((</>))

import qualified CommandWrapper.Config.Global as Global (Config(Config))
import qualified CommandWrapper.Config.Global as Global.Config
    ( Config(..)
    , def
    , read
    )
import CommandWrapper.Environment
    ( AppNames(..)
    , commandWrapperPrefix
    , getAppNames
    , parseEnvIO
    )
import qualified CommandWrapper.External as External
import qualified CommandWrapper.Internal as Internal
import qualified CommandWrapper.Options as Options
import CommandWrapper.Options.ColourOutput (ColourOutput)
import qualified CommandWrapper.Options.ColourOutput as ColourOutput
    ( options
    , noColorEnvVar
    )
import qualified CommandWrapper.Options.GlobalMode as Options

import Paths_command_wrapper (version)


readConfig :: Options.Command a -> IO (Either String (Endo Global.Config))
readConfig _ = pure (Right mempty)

defaults
    :: Global.Config
    -> Endo (Options.Command Global.Config)
    -> IO (Options.Command Global.Config)
defaults config (Endo f) =
    pure . f $ Mainplate.Internal (Internal.HelpCmommand []) config

main :: IO ()
main = do
    appNames@AppNames{exeName, usedName} <- getAppNames'
    -- TODO: It would be great to have debugging message with 'appNames' in it.
    defaultColourOutput <- parseEnvIO (die . show) ColourOutput.noColorEnvVar

    let defaultConfig = Global.Config.def
            { Global.Config.colourOutput = defaultColourOutput
            }

    -- TODO: This code can be simplified and generalised by mapping over a list
    --       of names under which command-wrapper is known at the moment.
    config <- (\f g -> g (f defaultConfig))
        <$> readGlobalConfig exeName
        <*> ( if exeName /= usedName
                 then readGlobalConfig usedName
                 else pure id
            )

    Mainplate.runExtensibleAppWith (parseOptions config) readConfig
        (defaults config) (External.run appNames) (Internal.run appNames)
  where
    readGlobalConfig baseName = do
        configFile <- getXdgDirectory XdgConfig (baseName </> "default.dhall")
        configExists <- doesFileExist configFile
        -- TODO: Debugging message that tells the user that we either read the
        --   configuration file or that we are using hardcoded defaults.
--      print (if configExists then "Reading" else "No such file", configFile)
        if configExists
            then
                Global.Config.read configFile >>= either die (pure . flip (<>))
            else
                pure id

getAppNames' :: IO AppNames
getAppNames' = getAppNames commandWrapperPrefix (pure version)

parseOptions :: Global.Config -> IO (Endo (Options.Command Global.Config))
parseOptions config =
    Options.parseCommandWrapper Options.defaultPrefs parserInfo
        $ pure . Global.Config.aliases . (`appEndo` config)
  where
    parserInfo :: Options.ParserInfo (Options.GlobalMode (Endo Global.Config))
    parserInfo =
        Options.info globalOptions Options.fullDesc

globalOptions :: Options.Parser (Options.GlobalMode (Endo Global.Config))
globalOptions = withHelpOptions
    <*> ( foldEndo
            <$> verbosityOptions
            <*> colourOptions
        )

withHelpOptions
    :: Options.Parser
        ( Endo Global.Config -> Options.GlobalMode (Endo Global.Config)
        )
withHelpOptions = Options.flag Options.PreserveMode Options.HelpMode $ mconcat
    [ Options.short 'h'
    , Options.long "help"
    , Options.help "Print this help message and exit."
    ]

verbosityOptions :: Options.Parser (Endo Global.Config)
verbosityOptions = foldEndo
    <$> many Options.incrementVerbosityFlag
    <*> optional Options.verbosityOption
    <*> Options.silentFlag
    <*> Options.quietFlag

colourOptions :: Options.Parser (Endo Global.Config)
colourOptions = modifyConfig <$> ColourOutput.options
  where
    modifyConfig :: Maybe ColourOutput -> Endo Global.Config
    modifyConfig newValue =
        Endo $ \config@Global.Config{Global.Config.colourOutput} -> config
            { Global.Config.colourOutput =
                getLast (Last colourOutput <> Last newValue)
            }
