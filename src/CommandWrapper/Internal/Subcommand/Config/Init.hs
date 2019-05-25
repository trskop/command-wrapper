{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:      CommandWrapper.Internal.Subcommand.Config.Init
-- Description: Initialisation capabilities of config subcommand.
-- Copyright:   (c) 2019 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Initialisation capabilities of config subcommand:
--
-- * Create toolset symbolic link.
-- * Create config and library directories.
-- * Create initial configuration for toolset and Command Wrapper's external
--   subcommands.
module CommandWrapper.Internal.Subcommand.Config.Init
    ( InitOptions(..)
    , defInitOptions
    , init
    )
  where

import Control.Applicative (pure)
import Control.Monad ((>>=), unless)
import Data.Bool (Bool(False, True), (||), not, otherwise)
import Data.Either (Either(Left, Right))
import Data.Eq ((==))
import Data.Foldable (for_)
import Data.Function (($), (.), const)
import Data.Functor (Functor, (<$>), (<&>))
import Data.Int (Int)
import qualified Data.List as List (intercalate)
import Data.Maybe (Maybe(Just, Nothing), fromMaybe)
import Data.Monoid ((<>))
import Data.String (String, fromString)
import GHC.Generics (Generic)
import System.Exit (ExitCode(ExitFailure), exitWith)
import System.IO (FilePath, IO, stderr, stdout)
import Text.Show (Show, show)

import Data.Text.Prettyprint.Doc ((<+>), pretty)
import qualified Data.Text.Prettyprint.Doc as Pretty
    ( Doc
    , brackets
    , hsep
    , line
    , squotes
    , vsep
    )
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Pretty (AnsiStyle)
import qualified Data.Text.Prettyprint.Doc.Util as Pretty (reflow)
import System.Directory
    ( XdgDirectory(XdgConfig)
    , createDirectoryIfMissing
    , doesDirectoryExist
    , findExecutable
    , getHomeDirectory
    , getHomeDirectory
    , getXdgDirectory
    )
import System.FilePath ((</>))
import System.Posix.Files (createSymbolicLink)

import CommandWrapper.Config.Global (Config(..))
import CommandWrapper.Environment (AppNames(AppNames, exePath, usedName))
import CommandWrapper.Internal.Subcommand.Help (command)
import CommandWrapper.Message
    ( Result
    , defaultLayoutOptions
    , errorMsg
    , message
    )
import qualified CommandWrapper.Options.ColourOutput as ColourOutput
    ( ColourOutput(Auto)
    )


data InitOptions = InitOptions
    { toolsetName :: String
    , binDir :: Maybe FilePath
    -- ^ Bin directory specified by the user.
    }
  deriving (Generic, Show)

defInitOptions
    :: String
    -- ^ Toolset name, it's not optional.
    -> InitOptions
defInitOptions toolsetName = InitOptions
    { toolsetName
    , binDir = Nothing
    }

init :: AppNames -> Config -> InitOptions -> IO ()
init
  AppNames{exePath, usedName}
  Config{colourOutput, verbosity}
  InitOptions{..} = do

    destination <- case binDir of
        Just dir -> do
            checkDir dir >>= \case
                Nothing -> do
                    dieWith 1
                        ( fromString (show dir)
                        <> ": Directory doesn't exist."
                        )
                Just d -> pure d

        Nothing -> do
            home <- getHomeDirectory
            let binDirs = (home </>) <$> [".local/bin", "bin"]
            checkDirs binDirs >>= \case
                Nothing -> do
                    dieWith 1
                        ( "None of these directories exist: "
                        <> fromString (unlist (show <$> binDirs))
                        )
                Just dir -> pure dir

    unless (usedName == "command-wrapper")
        $ findExecutable toolsetName >>= \case
            Nothing -> do
                let dst = destination </> toolsetName
                createSymbolicLink exePath dst
                messageLn
                    [ command (fromString dst) <> ":"
                    , Pretty.reflow "Symbolic link to"
                    , command (fromString exePath)
                    , "created successfully."
                    ]

            Just _ ->
                messageLn
                    [ command (fromString toolsetName) <> ":"
                    , Pretty.reflow
                        "Executable already exist, skipping symlinking"
                    , command (fromString exePath) <> "."
                    ]

    configDir <- getXdgDirectory XdgConfig toolsetName
    libDir <- (</> (".local/lib" </> toolsetName)) <$> getHomeDirectory
    dirs <- dirsExistence
        [ configDir
        , configDir </> "default"
        , configDir </> "cd"
        , configDir </> "exec"
        , configDir </> "skel"
        , libDir
        ]
    for_ dirs \case
        Left dir -> do
            createDirectoryIfMissing True dir
            messageLn
                [ command (fromString dir) <> ":"
                , Pretty.reflow "Directory created successfully."
                ]
        Right dir ->
            messageLn
                [ command (fromString dir) <> ":"
                , Pretty.reflow
                    "Directory already exists, skipping its creation."
                ]

    if (usedName == "command-wrapper")
        then
            pure ()
        else
            pure ()
  where
    colourOutput' = fromMaybe ColourOutput.Auto colourOutput

    dieWith :: Int -> (forall ann. Pretty.Doc ann) -> IO a
    dieWith exitCode msg = do
        let subcommand :: forall ann. Pretty.Doc ann
            subcommand = pretty (usedName <> " config")

        errorMsg subcommand verbosity colourOutput' stderr msg
        exitWith (ExitFailure exitCode)

    messageLn fragments =
        message defaultLayoutOptions verbosity colourOutput' stdout
            (Pretty.hsep fragments <> Pretty.line)

checkDir :: FilePath -> IO (Maybe FilePath)
checkDir dir = do
    doesExist <- doesDirectoryExist dir
    pure if doesExist
        then Just dir
        else Nothing

-- | Find first directory that exists, or return 'Nothing' of none.
checkDirs :: [FilePath] -> IO (Maybe FilePath)
checkDirs = \case
    [] -> pure Nothing
    dir : dirs ->
        checkDir dir >>= \case
            r@(Just _) -> pure r
            Nothing -> checkDirs dirs

dirsExistence :: [FilePath] -> IO [Either FilePath FilePath]
dirsExistence = \case
    [] -> pure []
    dir : dirs -> do
        doesExist <- doesDirectoryExist dir
        ((if doesExist then Right else Left) dir :) <$> dirsExistence dirs

unlist :: [String] -> String
unlist = List.intercalate ", "

-- ${configDir}/command-wrapper/
-- │
-- ├── README.md
-- │
-- ├── default/
-- │   ├── aliases-common.dhall
-- │   ├── aliases-local.dhall
-- │   └── help.dhall
-- ├── default.dhall
-- │
-- ├── cd/
-- │   ├── directories-common.dhall
-- │   ├── directories-local.dhall
-- │   └── finder.dhall
-- ├── command-wrapper-cd.dhall
-- │
-- ├── exec/
-- ├── command-wrapper-exec.dhall
-- │
-- ├── skel/
-- ├── command-wrapper-skel.dhall
-- │
-- ├── Types.dhall
-- └── library.dhall
--
-- ${configDir}/${toolset}/
-- │
-- ├── README.md
-- │
-- ├── default/
-- │   ├── aliases-common.dhall
-- │   ├── aliases-local.dhall
-- │   └── help.dhall
-- ├── default.dhall
-- │
-- └── toolset/                   <-- Maybe leave this to `skel`?
--     ├── dhall/
--     │   └── *.dhall
--     ├── haskell/
--     │   ├── app-${toolset}-${subcommand0}
--     │   │   └── Main.hs
--     │   ├── ...
--     │   ├── command-wrapper-toolset-${toolset}.cabal
--     │   └── {package.yaml,stack.yaml}
--     ├── scripts/
--     │   ├── ${toolset}-${subcommand0}
--     │   └── ...
--     ├── man/
--     │   ├── ${toolset}-${subcommand0}.1.md
--     │   ├── ...
--     │   ├── ${toolset}-${topic0}.{5,7}.md
--     │   └── ...
--     ├── README.md
--     ├── Shakefile.hs
--     └── install

-- TODO:
--
-- - Lib and config directories.
-- - Initial configuration.

