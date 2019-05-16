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

import Control.Applicative ((<*>), optional, pure)
import Control.Monad ((>>=), unless)
import Data.Bool (Bool(True), (||), not, otherwise)
import Data.Either (Either(Left, Right))
import Data.Eq ((==))
import Data.Foldable (asum, for_, null)
import Data.Function (($), (.), const)
import Data.Functor (Functor, (<$>), (<&>))
import Data.Int (Int)
import qualified Data.List as List (elem, filter, intercalate, isPrefixOf)
--import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Maybe (Maybe(Just, Nothing), fromMaybe)
import Data.Monoid (Endo(Endo), (<>), mconcat, mempty)
import Data.String (String, fromString)
import GHC.Generics (Generic)
import System.Exit (ExitCode(ExitFailure), exitWith)
import System.IO (FilePath, IO, stderr, stdout)
import Text.Show (Show, show)

import Data.Monoid.Endo.Fold (dualFoldEndo)
import Data.Text.Prettyprint.Doc ((<+>), pretty)
import qualified Data.Text.Prettyprint.Doc as Pretty
    ( Doc
    , hsep
    , line
    , squotes
    , vsep
    )
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Pretty (AnsiStyle)
import qualified Data.Text.Prettyprint.Doc.Util as Pretty (reflow)
import qualified Mainplate (applySimpleDefaults)
import qualified Options.Applicative as Options
    ( Parser
    , defaultPrefs
    , flag
    , info
    , long
    , metavar
    , short
    , strOption
    )
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

import qualified CommandWrapper.Config.Global as Global (Config(..))
import CommandWrapper.Environment (AppNames(AppNames, exePath, usedName))
import CommandWrapper.Internal.Subcommand.Help
    ( command
    , globalOptionsHelp
    , helpOptions
    , longOption
    , longOptionWithArgument
    , metavar
    , optionDescription
--  , optionalMetavar
    , section
    , toolsetCommand
    , usageSection
    )
import CommandWrapper.Internal.Utils (runMain)
import CommandWrapper.Message
    ( Result
    , defaultLayoutOptions
    , errorMsg
    , message
    )
import qualified CommandWrapper.Options.Optparse as Options
    ( internalSubcommandParse
--  , splitArguments
--  , splitArguments'
    )
import qualified CommandWrapper.Options.ColourOutput as ColourOutput
    ( ColourOutput(Auto)
    )


data ConfigMode a
    = Init InitOptions a
    | ConfigLib a
    | Dhall a
    | Help a
  deriving (Functor, Generic, Show)

data InitOptions = InitOptions
    { toolsetName :: String
    , binDir :: Maybe FilePath
    }
  deriving (Generic, Show)

defInitOptions :: String -> InitOptions
defInitOptions toolsetName = InitOptions
    { toolsetName
    , binDir = Nothing
    }

config :: AppNames -> [String] -> Global.Config -> IO ()
config appNames@AppNames{exePath, usedName} options globalConfig =
    runMain (parseOptions appNames globalConfig options) defaults $ \case
        Init InitOptions{..} cfg@Global.Config{colourOutput, verbosity} -> do
            let colourOutput' = fromMaybe ColourOutput.Auto colourOutput

                messageLn fragments =
                    message defaultLayoutOptions verbosity colourOutput' stdout
                        (Pretty.hsep fragments <> Pretty.line)


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
                            , Pretty.reflow "Executable already exist,\
                                \ skipping symlinking"
                            , command (fromString exePath) <> "."
                            ]

            configDir <- getXdgDirectory XdgConfig toolsetName
            libDir <- (</> (".local/lib" </> toolsetName)) <$> getHomeDirectory
            dirs <- dirsExistence [configDir, libDir]
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
                        , Pretty.reflow "Directory already exists, skipping\
                            \ its creation."
                        ]

            -- TODO:
            --
            -- - Lib and config directories.
            -- - Initial configuration.
            -- - Support for initialisation of core Command Wrapper
            --   configuration and directories.  Make `--toolset=NAME`
            --   Optional?

        ConfigLib _ -> pure ()

        Help Global.Config{colourOutput, verbosity} ->
            message defaultLayoutOptions verbosity
                (fromMaybe ColourOutput.Auto colourOutput) stdout
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
    defaults = Mainplate.applySimpleDefaults (Help globalConfig)

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

dirsExistence :: [FilePath] -> IO [Either FilePath FilePath]
dirsExistence = \case
    [] -> pure []
    dir : dirs -> do
        doesExist <- doesDirectoryExist dir
        ((if doesExist then Right else Left) dir :) <$> dirsExistence dirs

unlist :: [String] -> String
unlist = List.intercalate ", "

parseOptions
    :: AppNames
    -> Global.Config
    -> [String]
    -> IO (Endo (ConfigMode Global.Config))
parseOptions appNames@AppNames{usedName} globalConfig options = execParser
    [ dualFoldEndo
        <$> initFlag
        <*> optional toolsetOption

    , helpFlag

    , pure mempty
    ]
  where
    switchTo :: ConfigMode Global.Config -> Endo (ConfigMode Global.Config)
    switchTo = Endo . const

    switchToHelpMode, switchToInitMode :: Endo (ConfigMode Global.Config)

    switchToHelpMode = switchTo (Help globalConfig)
    switchToInitMode = switchTo (Init (defInitOptions usedName) globalConfig)

    initFlag :: Options.Parser (Endo (ConfigMode Global.Config))
    initFlag = Options.flag mempty switchToInitMode (Options.long "init")

    toolsetOption :: Options.Parser (Endo (ConfigMode Global.Config))
    toolsetOption =
        Options.strOption (Options.long "toolset" <> Options.metavar "NAME")
            <&> \toolsetName -> Endo \case
                    Init opts cfg ->
                        Init (opts :: InitOptions){toolsetName} cfg
                    mode ->
                        mode

    helpFlag :: Options.Parser (Endo (ConfigMode Global.Config))
    helpFlag = Options.flag mempty switchToHelpMode $ mconcat
        [ Options.short 'h'
        , Options.long "help"
        ]

    execParser
        :: [Options.Parser (Endo (ConfigMode Global.Config))]
        -> IO (Endo (ConfigMode Global.Config))
    execParser parser =
        Options.internalSubcommandParse appNames globalConfig "config"
            Options.defaultPrefs (Options.info (asum parser) mempty) options

configSubcommandHelp :: AppNames -> Pretty.Doc (Result Pretty.AnsiStyle)
configSubcommandHelp AppNames{usedName} = Pretty.vsep
    [ Pretty.reflow
        "Initialise, query, and update Command Wrapper toolset configuration."
    , ""

    , usageSection usedName
--      [ "config" <+> optionalMetavar "EXPRESSION"

        [ "config"
            <+> longOption "init"
            <+> longOptionWithArgument "toolset" "NAME"

        , "config" <+> helpOptions
        , "help config"
        ]

    , section "Options:"
--      [ optionDescription ["EPRESSION"]
--          [ "Dhall", metavar "EXPRESSION"
--          , Pretty.reflow "that will be applied to configuration."
--          ]

        [ optionDescription ["--init"]
            [ Pretty.reflow "Initialise configuration of a toolset."
            ]

        , optionDescription ["--toolset=NAME"]
            [ Pretty.reflow "When specified allong with", longOption "init"
            , Pretty.reflow "then configuration for toolset", metavar "NAME"
            , Pretty.reflow "is initialised."
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

    hadInit = "--init" `List.elem` wordsBeforePattern

    munless p x = if not p then x else mempty

    possibleOptions =
        munless (hadHelp || hadInit) ["--help", "-h", "--init"]
        <> munless (hadHelp || not hadInit) ["--toolset="]

    matchingOptions = List.filter (pat `List.isPrefixOf`) possibleOptions
