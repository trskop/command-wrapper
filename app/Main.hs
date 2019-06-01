{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
-- |
-- Module:       Main
-- Description:  Top-level executable of Command Wrapper.
-- Copyright:    (c) 2014-2019 Peter Trsko
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

import Control.Applicative ((<*>), (<|>), many, pure)
import Control.Monad ((>>=))
import Data.Bool (Bool(False, True))
import Data.Either (Either(Right), either)
import Data.Eq ((/=))
import Data.Function (($), (.), id)
import Data.Functor ((<$>))
import Data.Maybe (fromMaybe, maybe)
import Data.Monoid (Endo(Endo, appEndo), mconcat, mempty)
import Data.String (String)
import System.Exit (die)
import System.IO (IO{-, print-})
import Text.Show (show)

import Data.Monoid.Endo.Fold (dualFoldEndo)
import qualified Data.Text as Text (unpack)
import Data.Verbosity (Verbosity(Silent))
import Data.Verbosity.Class (setVerbosity)
import qualified Mainplate.Extensible as Mainplate
    ( Command(Internal)
    , runExtensibleAppWith
    )
import qualified Options.Applicative as Options
import qualified Options.Applicative.Standard as Options
import System.Directory
    ( XdgDirectory(XdgConfig)
    , doesFileExist
    , getXdgDirectory
    )
import System.FilePath ((</>), splitSearchPath)

import qualified CommandWrapper.Config.Global as Global (Config)
import qualified CommandWrapper.Config.Global as Global.Config
    ( Config(..)
    , def
    , getAliases
    )
import qualified CommandWrapper.Config.File as Config.File (apply, read)
import CommandWrapper.Environment
    ( AppNames(..)
    , CommandWrapperToolsetVarName(CommandWrapperPath)
    , commandWrapperToolsetVarName
    , defaultCommandWrapperPrefix
    , getAppNames
    , optionalVar
    , parseEnvIO
    )
import qualified CommandWrapper.External as External
import qualified CommandWrapper.Internal as Internal
import qualified CommandWrapper.Options as Options
import CommandWrapper.Options.ColourOutput (ColourOutput)
import qualified CommandWrapper.Options.ColourOutput as ColourOutput
    ( ColourOutput(Auto)
    , options
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
    pure . f $ Mainplate.Internal (Internal.HelpCommand []) config

main :: IO ()
main = do
    appNames@AppNames{exeName, usedName} <- getAppNames'
    -- TODO: It would be great to have debugging message with 'appNames' in it.
    defaultConfig <- parseEnv

    -- TODO: This code can be simplified and generalised by mapping over a list
    --       of names under which command-wrapper is known at the moment.
    config <- (\f g -> g (f defaultConfig))
        <$> readGlobalConfig exeName
        <*> ( if exeName /= usedName
                 then readGlobalConfig usedName
                 else pure id
            )

    Mainplate.runExtensibleAppWith (parseOptions appNames config) readConfig
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
                Config.File.read configFile >>= either die (pure . Config.File.apply)
            else
                pure id

getAppNames' :: IO AppNames
getAppNames' = getAppNames defaultCommandWrapperPrefix (pure version)

-- | Parse environment variables and produce default configuration that can be
-- later updated by configuration file and command line options.
parseEnv :: IO Global.Config
parseEnv = parseEnvIO (die . show) do
    defColour <- fromMaybe ColourOutput.Auto <$> ColourOutput.noColorEnvVar

    searchPathVar <- commandWrapperToolsetVarName CommandWrapperPath
    searchPath <- optionalVar searchPathVar

    pure (Global.Config.def defColour)
        { Global.Config.searchPath =
            maybe [] (splitSearchPath . Text.unpack) searchPath
        }

parseOptions
    :: AppNames
    -> Global.Config
    -> IO (Endo (Options.Command Global.Config))
parseOptions appNames config =
    Options.parseCommandWrapper appNames Options.defaultPrefs parserInfo
        $ pure . Global.Config.getAliases . (`appEndo` config)
  where
    parserInfo :: Options.ParserInfo (Options.GlobalMode (Endo Global.Config))
    parserInfo =
        Options.info globalOptions Options.fullDesc

globalOptions :: Options.Parser (Options.GlobalMode (Endo Global.Config))
globalOptions = withGlobalMode
    <*> ( dualFoldEndo
            <$> verbosityOptions
            <*> many colourOptions
            <*> many (aliasesFlag <|> noAliasesFlag)
        )

verbosityOptions :: Options.Parser (Endo Global.Config)
verbosityOptions = dualFoldEndo
    <$> many Options.incrementVerbosityFlag
    <*> many Options.verbosityOption
    <*> many (Options.flag' (setVerbosity Silent) Options.silent)
    <*> many (Options.flag' (setVerbosity Silent) Options.quiet)

colourOptions :: Options.Parser (Endo Global.Config)
colourOptions = modifyConfig <$> ColourOutput.options
  where
    modifyConfig :: ColourOutput -> Endo Global.Config
    modifyConfig colourOutput =
        Endo \config -> config{Global.Config.colourOutput}

aliasesFlag :: Options.Parser (Endo Global.Config)
aliasesFlag = Options.flag' modifyConfig $ mconcat
    [ Options.long "aliases"
    , Options.help "Use SUBCOMMAND aliases, dual to '--no-aliases'."
    ]
  where
    modifyConfig :: Endo Global.Config
    modifyConfig =
        Endo $ \config -> config{Global.Config.ignoreAliases = False}

noAliasesFlag :: Options.Parser (Endo Global.Config)
noAliasesFlag = Options.flag' modifyConfig $ mconcat
    [ Options.long "no-aliases"
    , Options.help "Ignore SUBCOMMAND aliases, dual to '--aliases'."
    ]
  where
    modifyConfig :: Endo Global.Config
    modifyConfig =
        Endo $ \config -> config{Global.Config.ignoreAliases = True}

withGlobalMode
    :: Options.Parser
        ( Endo Global.Config -> Options.GlobalMode (Endo Global.Config)
        )
withGlobalMode = run <$> go
  where
    go :: Options.Parser (Endo (Options.GlobalMode (Endo Global.Config)))
    go = dualFoldEndo
        <$> helpOption
        <*> versionOption

    run :: Endo (Options.GlobalMode (Endo Global.Config))
        -> Endo Global.Config
        -> Options.GlobalMode (Endo Global.Config)
    run (Endo f) = f . Options.PreserveMode

helpOption :: Options.Parser (Endo (Options.GlobalMode (Endo Global.Config)))
helpOption = Options.flag mempty switchToHelpMode $ mconcat
    [ Options.short 'h'
    , Options.long "help"
    , Options.help "Print this help message and exit."
    ]
  where
    switchToHelpMode = Endo (Options.switchGlobalMode Options.HelpMode)

versionOption
    :: Options.Parser (Endo (Options.GlobalMode (Endo Global.Config)))
versionOption = Options.flag mempty switchToVersionMode (Options.version True)
  where
    switchToVersionMode = Endo (Options.switchGlobalMode Options.VersionMode)
