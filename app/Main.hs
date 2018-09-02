{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NamedFieldPuns #-}
-- {-# LANGUAGE TypeApplications #-}
-- |
-- Module:       Main
-- Description:  TODO: Module description.
-- Copyright:    (c) 2014-2018 Peter Trsko
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  GHC specific language extensions.
--
-- TODO: Module description.
module Main (main)
  where

import Control.Applicative ((<*>), many, optional, pure)
import Control.Monad ((>>=))
import Data.Either (Either(Right), either)
import Data.Eq ((/=))
import Data.Function (($), (.), flip, id)
import Data.Functor -- ((<$>))
import Data.Semigroup ((<>))
import Data.Monoid (Endo(Endo, appEndo), mempty)
import Data.String (String)
import System.Exit (die)
import System.IO (IO{-, print-})

import qualified Options.Applicative as Options
import qualified Options.Applicative.Standard as Options
import Data.Monoid.Endo.Fold (foldEndo)
import qualified Mainplate.Extensible as Mainplate -- (runExtensibleAppWith)
import System.Directory
    ( XdgDirectory(XdgConfig)
    , doesFileExist
    , getXdgDirectory
    )
import System.FilePath ((</>))

import qualified CommandWrapper.Config as Global (Config)
import qualified CommandWrapper.Config as Global.Config (Config(..), def, read)
import CommandWrapper.Environment (AppNames(..), getAppNames)
import qualified CommandWrapper.External as External
import qualified CommandWrapper.Internal as Internal
import qualified CommandWrapper.Options as Options


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
    appNames@AppNames{exeName, usedName} <- getAppNames

    -- TODO: This code can be simplified and generalised by mapping over a list
    --       of names under which command-wrapper is known at the moment.
    config <- (\f g -> g (f Global.Config.def))
        <$> readGlobalConfig exeName
        <*> ( if exeName /= usedName
                 then readGlobalConfig usedName
                 else pure id
            )

    Mainplate.runExtensibleAppWith (parseOptions config) readConfig
        (defaults config) (External.run appNames) (Internal.run appNames)
  where
    parseOptions config =
        Options.parseCommandWrapper Options.defaultPrefs optionsParser
            $ pure . Global.Config.aliases . (`appEndo` config)

    optionsParser = Options.info verbosityOptions Options.fullDesc

    verbosityOptions :: Options.Parser (Endo Global.Config)
    verbosityOptions = foldEndo
        <$> many Options.incrementVerbosityFlag
        <*> optional Options.verbosityOption
        <*> Options.silentFlag

    readGlobalConfig baseName = do
        configFile <- getXdgDirectory XdgConfig (baseName </> "default.dhall")
        configExists <- doesFileExist configFile
--      print (if configExists then "Reading" else "No such file", configFile)
        if configExists
            then
                Global.Config.read configFile >>= either die (pure . flip (<>))
            else
                pure id
