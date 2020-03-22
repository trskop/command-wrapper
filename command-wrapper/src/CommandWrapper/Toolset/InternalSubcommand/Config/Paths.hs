-- |
-- Module:      $Header$
-- Description: Generate Dhall configuration file with XDG Directories.
-- Copyright:   (c) 2018-2020 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Generate Dhall configuration file with XDG Directories.
module CommandWrapper.Toolset.InternalSubcommand.Config.Paths
    ( Paths(..)
    , Toolset(..)
    , mk
    )
  where

import Control.Applicative ((<*>), liftA2, pure)
import Control.Monad ((>>=))
import Data.Eq ((==))
import Data.Bool (Bool(True), (||))
import Data.Function ((.))
import Data.Foldable (null)
import Data.Functor ((<$>), fmap)
import Data.Maybe (Maybe(Just), maybe)
import Data.String ({-IsString,-} fromString)
import GHC.Generics (Generic)
import System.Environment (lookupEnv)
import System.IO (IO)
import Text.Show (Show)

import CommandWrapper.Core.Environment (Params(Params, exePath, name))
import Data.Text (Text)
import qualified Dhall (Inject)
import System.Directory
    ( XdgDirectory(XdgCache, XdgConfig, XdgData)
--  , doesDirectoryExist
--  , doesFileExist
    , getHomeDirectory
    , getXdgDirectory
--  , listDirectory
    )
--import System.Directory.ProjectRoot (getProjectRootCurrent)
import System.FilePath ((</>))
--import System.Process (readProcess)


data Paths = Paths
    { configDir :: Text
    , cacheDir :: Text
    , dataDir :: Text
    , configFile :: Text
    , exeFile :: Text
    , toolset :: Toolset

    , tmpDir :: Maybe Text
    -- ^ Environment variable set by e.g.
    -- [pam_mktemp](https://www.openwall.com/pam/) it has similar capabilities
    -- as are requred from @$XDG_RUNTIME_DIR@.

    , editor :: Text

-- TODO:
--  , pager :: Text
    }
  deriving stock (Generic, Show)
  deriving anyclass (Dhall.Inject)

data Toolset = Toolset
    { configDir :: Text
    , configFile :: Text
    , exeFile :: Text
    , libDir :: Text
    }
  deriving stock (Generic, Show)
  deriving anyclass (Dhall.Inject)

mk :: Params -> IO Paths
mk Params{name, exePath} = do
    home <- getHomeDirectory
    cacheDir <- fromString <$> getXdgDirectory XdgCache ""
    dataDir <- fromString <$> getXdgDirectory XdgData ""
--  runtimeDir <- fmap fromString <$> lookupEnv "XDG_RUNTIME_DIR"

    toolset <- Toolset
        <$> (fromString <$> getXdgDirectory XdgConfig name)
        <*> (fromString <$> getXdgDirectory XdgConfig (name </> "default.dhall"))
        <*> pure (fromString exePath)
        <*> pure (fromString (home </> ".local/lib" </> name))

    configDir <- fromString <$> getXdgDirectory XdgConfig "command-wrapper"
    configFile <- fromString <$> getXdgDirectory XdgConfig ("command-wrapper" </> "default.dhall")
    let exeFile = fromString exePath

    let -- This default is used in all cases, however, in case of dumb terminal
        -- it should probably be 'ex'.
        defaultEditor = "vi"

    editor <- maybe defaultEditor fromString <$> do
        let lookupEditor = lookupEnv "EDITOR"

            lookupVisualThenEditor =
                lookupEnv "VISUAL" >>= maybe lookupEditor (pure . Just)

            checkIfDumbTerminal = liftA2 (||) null (== "dumb")

        -- https://unix.stackexchange.com/questions/4859/visual-vs-editor-what-s-the-difference
        isDumbTerminal <- maybe True checkIfDumbTerminal <$> lookupEnv "TERM"
        if isDumbTerminal
            then lookupEditor
            else lookupVisualThenEditor

        -- TODO:
        --
        -- - Support Debian/Ubuntu select-editor and sensible-editor
        --   functionality.
        -- - It may be useful to check that the command exists on the system
        --   and resolve it to a full path.

    tmpDir <- fmap fromString <$> lookupEnv "TMPDIR"

    pure Paths
        { configDir
        , configFile
        , exeFile

        , cacheDir
        , dataDir

        , editor
        , tmpDir

        , toolset
        }
