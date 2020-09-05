{-# LANGUAGE TemplateHaskell #-}
-- |
-- Module:      $Header$
-- Description: Initialisation capabilities of config subcommand.
-- Copyright:   (c) 2019-2020 Peter Trško
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
module CommandWrapper.Toolset.InternalSubcommand.Config.Init
    ( InitOptions(..)
    , defInitOptions
    , init
    , ConfigFile(..)
    , configFileContent
    )
  where

import Control.Applicative ((<*>), pure)
import Control.Monad ((>>=), unless, when)
import Data.Bool (Bool(True))
import Data.Either (Either(Left, Right))
import Data.Eq ((==))
import Data.Foldable (traverse_)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Int (Int)
import qualified Data.List as List (intercalate)
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Monoid ((<>))
import Data.String (String, fromString)
import GHC.Generics (Generic)
import System.Exit (ExitCode(ExitFailure), exitWith)
import System.IO (FilePath, IO, stderr, stdout)
import Text.Show (Show, show)

import Data.Either.Validation (validationToEither)
import Data.Text (Text)
import qualified Data.Text as Text (unlines)
import Data.Text.Prettyprint.Doc (pretty)
import qualified Data.Text.Prettyprint.Doc as Pretty (Doc, hsep, line)
import qualified Data.Text.Prettyprint.Doc.Util as Pretty (reflow)
import Dhall (ToDhall)
import qualified Dhall
    ( Decoder(extract)
    , auto
    )
import Dhall.TH (staticDhallExpression)
import System.AtomicWrite.Writer.Text as Text (atomicWriteFile)
import System.Directory
    ( XdgDirectory(XdgConfig, XdgData)
    , createDirectoryIfMissing
    , doesDirectoryExist
    , doesFileExist
    , findExecutable
    , getHomeDirectory
    , getHomeDirectory
    , getXdgDirectory
    )
import System.FilePath ((</>))
import System.Posix.Files (createSymbolicLink)

import CommandWrapper.Core.Environment (AppNames(AppNames, exePath, usedName))
import CommandWrapper.Core.Help.Pretty (command)
import CommandWrapper.Core.Message (errorMsg, out)
import CommandWrapper.Toolset.Config.Global
    ( Config(Config, colourOutput, verbosity)
    )
import qualified CommandWrapper.Toolset.InternalSubcommand.Config.Dhall as Dhall
    ( Freeze(input, output)
    , Input(InputFile)
    , Output(OutputBasedOnInput)
    , OutputOrCheck(Write)
    , defFreeze
    , freeze
    )


data InitOptions = InitOptions
    { toolsetName :: String
    -- ^ Toolset which we are initialising.
    , binDir :: Maybe FilePath
    -- ^ Bin directory specified by the user.
    , configDir :: Maybe FilePath
    -- ^ Configuration directory specified by the user.  Toolset name is
    -- appended to it: @'configDir' '</>' 'toolsetName'@.
    , libexecDir :: Maybe FilePath
    -- ^ Directory for subcommand executables as specified by the user.  By
    -- default @~\/.local\/lib\/${toolsetName}@ is used.
    , manDir :: Maybe FilePath
    -- ^ Directory for manual pages as specified by the user.  By default @man@
    -- directory in XDG data directory is used.
    }
  deriving stock (Generic, Show)

defInitOptions
    :: String
    -- ^ Toolset name, it's not optional.
    -> InitOptions
defInitOptions toolsetName = InitOptions
    { toolsetName
    , binDir = Nothing
    , configDir = Nothing
    , libexecDir = Nothing
    , manDir = Nothing
    }

init :: AppNames -> Config -> InitOptions -> IO ()
init
  appNames@AppNames{exePath, usedName}
  config@Config{colourOutput, verbosity}
  InitOptions
    { toolsetName
    , binDir
    , configDir = configDir'
    , libexecDir
    , manDir = manDir'
    }
  = do
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

    unless (toolsetName == "command-wrapper")
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

    configDir <- maybe
        (getXdgDirectory XdgConfig toolsetName)
        (\d -> pure (d </> toolsetName))
        configDir'
    libDir <- maybe
        ((</> (".local/lib" </> toolsetName)) <$> getHomeDirectory)
        (\d -> pure (d </> toolsetName))
        libexecDir
    manDir <- maybe
        (getXdgDirectory XdgData ("man" </> toolsetName))
        pure
        manDir'
    let defaultConfigDir = configDir </> "default"
    dirsExistence [configDir, defaultConfigDir, libDir, manDir]
        >>= createOrSkipDirectories

    templates <- getConfigTemplates appNames config
    let configFileContent' = configFileContent RuntimePaths{..} templates

    let readmeFile = configDir </> "README.md"
    haveReadmeFile <- doesFileExist readmeFile
    if haveReadmeFile
        then
            messageLn
                [ command (fromString readmeFile) <> ":"
                , Pretty.reflow
                    "File already exists, skipping its creation."
                ]
        else do

            Text.atomicWriteFile readmeFile
                (configFileContent' (ReadmeMd toolsetName))

            messageLn
                [ command (fromString readmeFile) <> ":"
                , Pretty.reflow "File created."
                ]

    let defaultConfig = configDir </> "default.dhall"
        commonAliasesConfig = defaultConfigDir </> "aliases-common.dhall"
        commonHelpTxt = defaultConfigDir </> "help-common.txt"

    checkFile defaultConfig
        >>= createOrSkipFile
                (configFileContent' (DefaultConfig toolsetName))

    checkFile commonAliasesConfig
        >>= createOrSkipFile
                (configFileContent' (CommonAliasesConfig toolsetName))

    checkFile commonHelpTxt
        >>= createOrSkipFile
                (configFileContent' (CommonHelpTxt toolsetName))

    let cdConfigDir = configDir </> "cd"
        execConfigDir = configDir </> "exec"
        skelConfigDir = configDir </> "skel"

        cdConfig = configDir </> "command-wrapper-cd.dhall"
        execConfig = configDir </> "command-wrapper-exec.dhall"
        skelConfig = configDir </> "command-wrapper-skel.dhall"

        commonDirsConfig = cdConfigDir </> "directories-common.dhall"
        commonCommandsConfig = execConfigDir </> "commands-common.dhall"

    dirsExistence [cdConfigDir, execConfigDir, skelConfigDir]
        >>= createOrSkipDirectories

    checkFile cdConfig
        >>= createOrSkipFile (configFileContent' (CdConfig toolsetName))

    checkFile commonDirsConfig
        >>= createOrSkipFile
                (configFileContent' (CommonDirectoriesConfig toolsetName))

    checkFile execConfig
        >>= createOrSkipFile (configFileContent' (ExecConfig toolsetName))

    checkFile commonCommandsConfig
        >>= createOrSkipFile
                (configFileContent' (CommonCommandsConfig toolsetName))

    checkFile skelConfig
        >>= createOrSkipFile (configFileContent' (SkelConfig toolsetName))

    when (toolsetName == "command-wrapper") do

        let libraryDhall = configDir </> "library.dhall"
            execLibraryDhall = configDir </> "exec" </> "library.dhall"

        checkFile libraryDhall
            >>= createOrSkipFile (configFileContent' Library)

        -- TODO: Freeze only when created.
        Dhall.freeze appNames config Dhall.defFreeze
            { Dhall.input = Dhall.InputFile libraryDhall
            , Dhall.output = Dhall.Write Dhall.OutputBasedOnInput
            }

        checkFile execLibraryDhall
            >>= createOrSkipFile (configFileContent' ExecLibrary)

        -- TODO: Freeze only when created.
        Dhall.freeze appNames config Dhall.defFreeze
            { Dhall.input = Dhall.InputFile execLibraryDhall
            , Dhall.output = Dhall.Write Dhall.OutputBasedOnInput
            }
  where
    dieWith :: Int -> (forall ann. Pretty.Doc ann) -> IO a
    dieWith exitCode msg = do
        let subcommand :: forall ann. Pretty.Doc ann
            subcommand = pretty (usedName <> " config")

        errorMsg subcommand verbosity colourOutput stderr msg
        exitWith (ExitFailure exitCode)

    messageLn fragments =
        out verbosity colourOutput stdout (Pretty.hsep fragments <> Pretty.line)

    createOrSkipDirectories :: [Either FilePath FilePath] -> IO ()
    createOrSkipDirectories = traverse_ \case
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

    createOrSkipFile :: Text -> Either FilePath FilePath -> IO ()
    createOrSkipFile content = \case
        Left file -> do
            Text.atomicWriteFile file content
            messageLn
                [ command (fromString file) <> ":"
                , Pretty.reflow "File created successfully."
                ]

        Right file ->
            messageLn
                [ command (fromString file) <> ":"
                , Pretty.reflow
                    "File already exists, skipping its creation."
                ]

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

checkFile :: FilePath -> IO (Either FilePath FilePath)
checkFile file = do
    doesExist <- doesFileExist file
    pure if doesExist
        then Right file
        else Left file

data ConfigFile
    = DefaultConfig String
    | CommonAliasesConfig String
    | CommonHelpTxt String
    | CdConfig String
    | CommonDirectoriesConfig String
    | ExecLibrary
    | ExecConfig String
    | CommonCommandsConfig String
    | SkelConfig String
    | Library
    | ReadmeMd String

data DhallLibraries = DhallLibraries
    { commandWrapper :: FilePath
    , exec :: FilePath
    }
  deriving stock (Generic)
  deriving anyclass (ToDhall)

data RuntimePaths = RuntimePaths
    { libDir :: FilePath
    , manDir :: FilePath
    }
  deriving stock (Generic)
  deriving anyclass (ToDhall)

data ConfigTemplates = ConfigTemplates
    { readmeMd :: DhallLibraries -> RuntimePaths -> Text
    , readmeMdToolset :: Text -> DhallLibraries -> RuntimePaths -> Text
    , defaultConfig :: DhallLibraries -> RuntimePaths -> Text
    , defaultToolsetConfig :: Text -> DhallLibraries -> RuntimePaths -> Text
    , commonAliasesConfig :: DhallLibraries -> RuntimePaths -> Text
    , commonAliasesToolsetConfig :: Text -> DhallLibraries -> RuntimePaths -> Text
    , commonHelp :: DhallLibraries -> RuntimePaths -> Text
    , commonHelpToolset :: Text -> DhallLibraries -> RuntimePaths -> Text
    , cdConfig :: DhallLibraries -> RuntimePaths -> Text
    , cdToolsetConfig :: Text -> DhallLibraries -> RuntimePaths -> Text
    , commonDirectoriesConfig :: DhallLibraries -> RuntimePaths -> Text
    , commonDirectoriesToolsetConfig :: Text -> DhallLibraries -> RuntimePaths -> Text
    , execConfig :: DhallLibraries -> RuntimePaths -> Text
    , execToolsetConfig :: Text -> DhallLibraries -> RuntimePaths -> Text
    , commonCommandsConfig :: DhallLibraries -> RuntimePaths -> Text
    , commonCommandsToolsetConfig :: Text -> DhallLibraries -> RuntimePaths -> Text
    , skelConfig :: DhallLibraries -> RuntimePaths -> Text
    , skelToolsetConfig :: Text -> DhallLibraries -> RuntimePaths -> Text
    }

getConfigTemplates :: AppNames -> Config -> IO ConfigTemplates
getConfigTemplates AppNames{usedName} Config{colourOutput, verbosity} =
    handleErrors
        ( ConfigTemplates
            <$> readmeMd
            <*> readmeMdToolset
            <*> defaultConfig
            <*> defaultToolsetConfig
            <*> commonAliasesConfig
            <*> commonAliasesToolsetConfig
            <*> commonHelp
            <*> commonHelpToolset
            <*> cdConfig
            <*> cdToolsetConfig
            <*> commonDirectoriesConfig
            <*> commonDirectoriesToolsetConfig
            <*> execConfig
            <*> execToolsetConfig
            <*> commonCommandsConfig
            <*> commonCommandsToolsetConfig
            <*> skelConfig
            <*> skelToolsetConfig
        )
  where
    handleErrors a = case validationToEither a of
        Left e ->
            dieWith 1 (fromString (show e))

        Right r ->
            pure r

    readmeMd = Dhall.extract Dhall.auto
        $( staticDhallExpression
            "./dhall/init/command-wrapper/README.md.dhall"
         )

    readmeMdToolset = Dhall.extract Dhall.auto
        $( staticDhallExpression
            "./dhall/init/toolset/README.md.dhall"
         )

    defaultConfig = Dhall.extract Dhall.auto
        $( staticDhallExpression
            "./dhall/init/command-wrapper/default.dhall"
         )

    defaultToolsetConfig = Dhall.extract Dhall.auto
        $( staticDhallExpression
            "./dhall/init/toolset/default.dhall"
         )

    commonAliasesConfig = Dhall.extract Dhall.auto
        $( staticDhallExpression
            "./dhall/init/command-wrapper/default/aliases-common.dhall"
         )

    commonAliasesToolsetConfig = Dhall.extract Dhall.auto
        $( staticDhallExpression
            "./dhall/init/toolset/default/aliases-common.dhall"
         )

    commonHelp = Dhall.extract Dhall.auto
        $( staticDhallExpression
            "./dhall/init/command-wrapper/default/help-common.dhall"
         )

    commonHelpToolset = Dhall.extract Dhall.auto
        $( staticDhallExpression
            "./dhall/init/toolset/default/help-common.dhall"
         )

    cdConfig = Dhall.extract Dhall.auto
        $( staticDhallExpression
            "./dhall/init/command-wrapper/command-wrapper-cd.dhall"
         )

    cdToolsetConfig = Dhall.extract Dhall.auto
        $( staticDhallExpression
            "./dhall/init/toolset/command-wrapper-cd.dhall"
         )

    commonDirectoriesConfig = Dhall.extract Dhall.auto
        $( staticDhallExpression
            "./dhall/init/command-wrapper/cd/directories-common.dhall"
         )

    commonDirectoriesToolsetConfig = Dhall.extract Dhall.auto
        $( staticDhallExpression
            "./dhall/init/toolset/cd/directories-common.dhall"
         )

    execConfig = Dhall.extract Dhall.auto
        $( staticDhallExpression
            "./dhall/init/command-wrapper/command-wrapper-exec.dhall"
         )

    execToolsetConfig = Dhall.extract Dhall.auto
        $( staticDhallExpression
            "./dhall/init/toolset/command-wrapper-exec.dhall"
         )

    commonCommandsConfig = Dhall.extract Dhall.auto
        $( staticDhallExpression
            "./dhall/init/command-wrapper/exec/commands-common.dhall"
         )

    commonCommandsToolsetConfig = Dhall.extract Dhall.auto
        $( staticDhallExpression
            "./dhall/init/toolset/exec/commands-common.dhall"
         )

    skelConfig = Dhall.extract Dhall.auto
        $( staticDhallExpression
            "./dhall/init/command-wrapper/command-wrapper-skel.dhall"
         )

    skelToolsetConfig = Dhall.extract Dhall.auto
        $( staticDhallExpression
            "./dhall/init/toolset/command-wrapper-skel.dhall"
         )

    dieWith :: Int -> (forall ann. Pretty.Doc ann) -> IO a
    dieWith exitCode msg = do
        let subcommand :: forall ann. Pretty.Doc ann
            subcommand = pretty (usedName <> " config")

        errorMsg subcommand verbosity colourOutput stderr msg
        exitWith (ExitFailure exitCode)

configFileContent :: RuntimePaths -> ConfigTemplates -> ConfigFile -> Text
configFileContent runtimePaths ConfigTemplates{..} = \case
    -- ${CONFIG_DIR}/command-wrapper/default.dhall
    DefaultConfig "command-wrapper" ->
        defaultConfig
            DhallLibraries
                { commandWrapper = "./library.dhall"
                , exec = "./exec/library.dhall"
                }
            runtimePaths

    -- ${CONFIG_DIR}/${toolsetName}/default.dhall
    DefaultConfig toolsetName ->
        defaultToolsetConfig
            (fromString toolsetName)
            DhallLibraries
                { commandWrapper = "../command-wrapper/library.dhall"
                , exec = "../command-wrapper/exec/library.dhall"
                }
            runtimePaths

    -- ${CONFIG_DIR}/command-wrapper/default/aliases-common.dhall
    CommonAliasesConfig "command-wrapper" ->
        commonAliasesConfig
            DhallLibraries
                { commandWrapper = "../library.dhall"
                , exec = "../exec/library.dhall"
                }
            runtimePaths

    -- ${CONFIG_DIR}/${toolsetName}/default/aliases-common.dhall
    CommonAliasesConfig toolsetName ->
        commonAliasesToolsetConfig
            (fromString toolsetName)
            DhallLibraries
                { commandWrapper = "../../command-wrapper/library.dhall"
                , exec = "../../command-wrapper/exec/library.dhall"
                }
            runtimePaths

    -- ${CONFIG_DIR}/command-wrapper/comman-wrapper-cd.dhall
    CdConfig "command-wrapper" ->
        cdConfig
            DhallLibraries
                { commandWrapper = "./library.dhall"
                , exec = "./exec/library.dhall"
                }
            runtimePaths

    -- ${CONFIG_DIR}/${toolsetName}/comman-wrapper-cd.dhall
    CdConfig toolsetName ->
        cdToolsetConfig
            (fromString toolsetName)
            DhallLibraries
                { commandWrapper = "../command-wrapper/library.dhall"
                , exec = "../command-wrapper/exec/library.dhall"
                }
            runtimePaths

    -- ${CONFIG_DIR}/comman-wrapper/cd/directories-common.dhall
    CommonDirectoriesConfig "command-wrapper" ->
        commonDirectoriesConfig
            DhallLibraries
                { commandWrapper = "../library.dhall"
                , exec = "../exec/library.dhall"
                }
            runtimePaths

    -- ${CONFIG_DIR}/${toolsetName}/cd/directories-common.dhall
    CommonDirectoriesConfig toolsetName ->
        commonDirectoriesToolsetConfig
            (fromString toolsetName)
            DhallLibraries
                { commandWrapper = "../../command-wrapper/library.dhall"
                , exec = "../../command-wrapper/exec/library.dhall"
                }
            runtimePaths

    -- ${CONFIG_DIR}/comman-wrapper/default/help-common.dhall
    CommonHelpTxt "command-wrapper" ->
        commonHelp
            DhallLibraries
                { commandWrapper = "../library.dhall"
                , exec = "../exec/library.dhall"
                }
            runtimePaths

    -- ${CONFIG_DIR}/${toolsetName}/default/help-common.dhall
    CommonHelpTxt toolsetName ->
        commonHelpToolset
            (fromString toolsetName)
            DhallLibraries
                { commandWrapper = "../../command-wrapper/library.dhall"
                , exec = "../../command-wrapper/exec/library.dhall"
                }
            runtimePaths

    -- ${CONFIG_DIR}/command-wrapper/exec/library.dhall
    ExecLibrary -> Text.unlines
        -- TODO: Use the same import as provided by:
        -- ```
        -- completion --library --dhall=exec --import
        -- ```
        [ "https://raw.githubusercontent.com/trskop/command-wrapper/master/dhall/Exec/package.dhall"
        ]

    -- ${CONFIG_DIR}/command-wrapper/command-wrapper-exec.dhall
    ExecConfig "command-wrapper" ->
        execConfig
            DhallLibraries
                { commandWrapper = "./library.dhall"
                , exec = "./exec/library.dhall"
                }
            runtimePaths

    -- ${CONFIG_DIR}/${toolsetName}/command-wrapper-exec.dhall
    ExecConfig toolsetName ->
        execToolsetConfig
            (fromString toolsetName)
            DhallLibraries
                { commandWrapper = "../command-wrapper/library.dhall"
                , exec = "../command-wrapper/exec/library.dhall"
                }
            runtimePaths

    -- ${CONFIG_DIR}/command-wrapper/exec/commands-common.dhall
    CommonCommandsConfig "command-wrapper" ->
        commonCommandsConfig
            DhallLibraries
                { commandWrapper = "../library.dhall"
                , exec = "./library.dhall"
                }
            runtimePaths

    -- ${CONFIG_DIR}/${toolsetName}/exec/commands-common.dhall
    CommonCommandsConfig toolsetName ->
        commonCommandsToolsetConfig
            (fromString toolsetName)
            DhallLibraries
                { commandWrapper = "../../command-wrapper/library.dhall"
                , exec = "../../command-wrapper/exec/library.dhall"
                }
            runtimePaths

    -- ${CONFIG_DIR}/command-wrapper/command-wrapper-skel.dhall
    SkelConfig "command-wrapper" ->
        skelConfig
            DhallLibraries
                { commandWrapper = "./library.dhall"
                , exec = "./exec/library.dhall"
                }
            runtimePaths

    -- ${CONFIG_DIR}/${toolsetName}/command-wrapper-skel.dhall
    SkelConfig toolsetName ->
        skelToolsetConfig
            (fromString toolsetName)
            DhallLibraries
                { commandWrapper = "../command-wrapper/library.dhall"
                , exec = "../command-wrapper/exec/library.dhall"
                }
            runtimePaths

    -- ${CONFIG_DIR}/command-wrapper/library.dhall
    Library -> Text.unlines
        -- TODO: Use the same import as provided by:
        -- ```
        -- completion --library --dhall=command-wrapper --import
        -- ```
        [ "https://raw.githubusercontent.com/trskop/command-wrapper/master/dhall/CommandWrapper/package.dhall"
        ]

    -- ${CONFIG_DIR}/command-wrapper/README.md
    ReadmeMd "command-wrapper" ->
        readmeMd
            DhallLibraries
                { commandWrapper = "./library.dhall"
                , exec = "./exec/library.dhall"
                }
            runtimePaths

    -- ${CONFIG_DIR}/${toolsetName}/README.md
    ReadmeMd toolsetName ->
        readmeMdToolset
            (fromString toolsetName)
            DhallLibraries
                { commandWrapper = "./library.dhall"
                , exec = "./exec/library.dhall"
                }
            runtimePaths
