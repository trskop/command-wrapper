{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:      CommandWrapper.Internal
-- Description: Implementation of internal command named config
-- Copyright:   (c) 2018-2019 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Implementation of internal command named @config@.
module CommandWrapper.Internal.Subcommand.Config
    ( ConfigMode(..)
    , config
    , configSubcommandHelp
    , configCompletion
    )
  where

import Control.Applicative (pure)
import Control.Monad ((>>=))
import Data.Bool ((||), not, otherwise)
import Data.Foldable (null)
import Data.Function (($))
import Data.Functor (Functor, (<$>))
import Data.Int (Int)
import qualified Data.List as List (elem, filter, intercalate, isPrefixOf)
--import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Maybe (Maybe(Just, Nothing), fromMaybe)
import Data.Monoid (Endo, (<>), mempty)
import Data.String (String, fromString)
import GHC.Generics (Generic)
import System.Exit (ExitCode(ExitFailure), exitWith)
import System.IO (FilePath, IO, stderr, stdout)
import Text.Show (Show, show)

import Data.Text.Prettyprint.Doc ((<+>), pretty)
import qualified Data.Text.Prettyprint.Doc as Pretty (Doc, squotes, vsep)
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Pretty (AnsiStyle)
import qualified Data.Text.Prettyprint.Doc.Util as Pretty (reflow)
import qualified Mainplate (applySimpleDefaults)
import System.Directory (doesDirectoryExist, getHomeDirectory)
import System.FilePath ((</>))
import System.Posix.Files (createSymbolicLink)

import qualified CommandWrapper.Config.Global as Global (Config(..))
import CommandWrapper.Environment (AppNames(AppNames, exePath, usedName))
import CommandWrapper.Internal.Subcommand.Help
    ( globalOptionsHelp
    , helpOptions
    , longOption
    , longOptionWithArgument
    , metavar
    , optionDescription
    , optionalMetavar
    , section
    , toolsetCommand
    , usageSection
    )
import CommandWrapper.Internal.Utils (runMain)
import CommandWrapper.Message
    ( Result
    , defaultLayoutOptions
    , dieSubcommandNotYetImplemented
    , errorMsg
    , message
    )
import qualified CommandWrapper.Options.ColourOutput as ColourOutput
    ( ColourOutput(Auto)
    )


data ConfigMode a
    = InitConfig InitOptions a
    | ConfigLib a
    | Dhall a
    | ConfigHelp a
  deriving (Functor, Generic, Show)

data InitOptions = InitOptions
    { toolsetName :: String
    , binDir :: Maybe FilePath
    }
  deriving (Generic, Show)

config :: AppNames -> [String] -> Global.Config -> IO ()
config appNames@AppNames{exePath, usedName} _options globalConfig =
    runMain (parseOptions appNames globalConfig) defaults $ \case
        InitConfig InitOptions{..} cfg -> do
            destination <- case binDir of
                Just dir -> do
                    checkDir dir >>= \case
                        Nothing -> do
                            dieWith cfg 1
                                ( fromString (show dir)
                                <> ": Directory doesn't exist."
                                )
                        Just d -> pure d

                Nothing -> do
                    home <- getHomeDirectory
                    let binDirs = (home </>) <$> [".local/bin", "bin"]
                    checkDirs binDirs >>= \case
                        Nothing -> do
                            dieWith cfg 1
                                ( "None of these directories exist: "
                                <> fromString (unlist (show <$> binDirs))
                                )
                        Just dir -> pure dir

            createSymbolicLink exePath (destination </> toolsetName)

            -- TODO:
            --
            -- - Lib and config directories.
            -- - Initial configuration.
            -- - Support for initialisation of core Command Wrapper
            --   configuration and directories.  Make `--toolset=NAME`
            --   Optional?

        ConfigLib _ -> pure ()

        ConfigHelp cfg ->
            let Global.Config{colourOutput, verbosity} = cfg
                colour = fromMaybe ColourOutput.Auto colourOutput
            in message defaultLayoutOptions verbosity colour stdout
                (configSubcommandHelp appNames)

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
    defaults = Mainplate.applySimpleDefaults (ConfigHelp globalConfig)

    dieWith :: Global.Config -> Int -> (forall ann. Pretty.Doc ann) -> IO a
    dieWith cfg exitCode msg = do
        let Global.Config{colourOutput, verbosity} = cfg
            colour = fromMaybe ColourOutput.Auto colourOutput

            subcommand :: forall ann. Pretty.Doc ann
            subcommand = pretty (usedName <> " config")

        errorMsg subcommand verbosity colour stderr msg
        exitWith (ExitFailure exitCode)

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

unlist :: [String] -> String
unlist = List.intercalate ", "

parseOptions
    :: AppNames
    -> Global.Config
    -> IO (Endo (ConfigMode Global.Config))
parseOptions AppNames{usedName} globalConfig =
    let Global.Config
            { Global.verbosity
            , Global.colourOutput = possiblyColourOutput
            } = globalConfig

    in dieSubcommandNotYetImplemented (fromString usedName) verbosity
        (fromMaybe ColourOutput.Auto possiblyColourOutput) stderr "config"

configSubcommandHelp :: AppNames -> Pretty.Doc (Result Pretty.AnsiStyle)
configSubcommandHelp AppNames{usedName} = Pretty.vsep
    [ usageSection usedName
        [ "config" <+> optionalMetavar "EXPRESSION"

        , "config"
            <+> longOption "init"
            <+> longOptionWithArgument "toolset" "NAME"

        , "config" <+> helpOptions
        , "help config"
        ]

    , section "Options:"
        [ optionDescription ["EPRESSION"]
            [ "Dhall", metavar "EXPRESSION"
            , Pretty.reflow "that will be applied to configuration."
            ]

        , optionDescription ["--help", "-h"]
            [ Pretty.reflow "Print this help and exit. Same as"
            , Pretty.squotes (toolsetCommand usedName "help config") <> "."
            ]

        , globalOptionsHelp usedName
        ]

    , ""
    ]

configCompletion
    :: AppNames
    -> Global.Config
    -> [String]
    -> String
    -> IO [String]
configCompletion _appNames _config wordsBeforePattern pat
  | null pat  = pure possibleOptions
  | otherwise = pure matchingOptions
  where
    hadHelp =
        ("--help" `List.elem` wordsBeforePattern)
        || ("-h" `List.elem` wordsBeforePattern)

    munless p x = if not p then x else mempty

    possibleOptions = munless hadHelp ["--help", "-h"]

    matchingOptions = List.filter (pat `List.isPrefixOf`) possibleOptions
