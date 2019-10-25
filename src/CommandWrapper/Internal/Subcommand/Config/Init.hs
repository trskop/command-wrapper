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
    , ConfigFile(..)
    , configFileContent
    )
  where

import Control.Applicative (pure)
import Control.Monad ((>>=), unless, when)
import Data.Bool (Bool(True))
import Data.Either (Either(Left, Right))
import Data.Eq ((==))
import Data.Foldable ({-for_,-} traverse_)
import Data.Function (($), (.))
import Data.Functor ((<$>))
import Data.Int (Int)
import qualified Data.List as List (intercalate)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Monoid ((<>))
import Data.String (String, fromString)
import GHC.Generics (Generic)
import System.Exit (ExitCode(ExitFailure), exitWith)
import System.IO (FilePath, IO, stderr, stdout)
import Text.Show (Show, show)

import Data.Text (Text)
import qualified Data.Text as Text (unlines)
import qualified Data.Text.IO as Text (writeFile)
import Data.Text.Prettyprint.Doc (pretty)
import qualified Data.Text.Prettyprint.Doc as Pretty (Doc, hsep, line)
import qualified Data.Text.Prettyprint.Doc.Util as Pretty (reflow)
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
--import qualified Dhall.TH (staticDhallExpression)
import System.FilePath ((</>))
import System.Posix.Files (createSymbolicLink)

import CommandWrapper.Config.Global (Config(..))
import CommandWrapper.Environment (AppNames(AppNames, exePath, usedName))
import qualified CommandWrapper.Internal.Subcommand.Config.Dhall as Dhall
    ( Freeze(input, output)
    , Input(InputFile)
    , Output(OutputBasedOnInput)
    , defFreeze
    , freeze
    )
import CommandWrapper.Internal.Subcommand.Help (command)
import CommandWrapper.Message (errorMsg, out)


data InitOptions = InitOptions
    { toolsetName :: String
    , binDir :: Maybe FilePath
    -- ^ Bin directory specified by the user.
    }
  deriving stock (Generic, Show)

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
  appNames@AppNames{exePath, usedName}
  config@Config{colourOutput, verbosity}
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

    configDir <- getXdgDirectory XdgConfig toolsetName
    libDir <- (</> (".local/lib" </> toolsetName)) <$> getHomeDirectory
    manDir <- getXdgDirectory XdgData ("man" </> toolsetName)
    let defaultConfigDir = configDir </> "default"
    dirsExistence [configDir, defaultConfigDir, libDir]
        >>= createOrSkipDirectories

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
            Text.writeFile readmeFile (readmeFileContent toolsetName)
            messageLn
                [ command (fromString readmeFile) <> ":"
                , Pretty.reflow "File created."
                ]

    let defaultConfig = configDir </> "default.dhall"
        commonAliasesConfig = defaultConfigDir </> "aliases-common.dhall"
        commonHelpTxt = defaultConfigDir </> "help-common.txt"

    checkFile defaultConfig
        >>= createOrSkipFile
                (configFileContent (DefaultConfig toolsetName libDir manDir))

    checkFile commonAliasesConfig
        >>= createOrSkipFile
                (configFileContent (CommonAliasesConfig toolsetName))

    checkFile commonHelpTxt
        >>= createOrSkipFile
                (configFileContent (CommonHelpTxt toolsetName))

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
        >>= createOrSkipFile (configFileContent (CdConfig toolsetName))

    checkFile commonDirsConfig
        >>= createOrSkipFile
                (configFileContent (CommonDirectoriesConfig toolsetName))

    checkFile execConfig
        >>= createOrSkipFile (configFileContent (ExecConfig toolsetName))

    checkFile commonCommandsConfig
        >>= createOrSkipFile
                (configFileContent (CommonCommandsConfig toolsetName))

    checkFile skelConfig
        >>= createOrSkipFile (configFileContent (SkelConfig toolsetName))

    when (toolsetName == "command-wrapper") do

        let libraryDhall = configDir </> "library.dhall"
            execLibraryDhall = configDir </> "exec" </> "library.dhall"

        checkFile libraryDhall
            >>= createOrSkipFile (configFileContent Library)

        -- TODO: Freeze only when created.
        Dhall.freeze appNames config Dhall.defFreeze
            { Dhall.input = Dhall.InputFile libraryDhall
            , Dhall.output = Dhall.OutputBasedOnInput
            }

        checkFile execLibraryDhall
            >>= createOrSkipFile (configFileContent ExecLibrary)

        -- TODO: Freeze only when created.
        Dhall.freeze appNames config Dhall.defFreeze
            { Dhall.input = Dhall.InputFile execLibraryDhall
            , Dhall.output = Dhall.OutputBasedOnInput
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
            Text.writeFile file content
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

readmeFileContent :: String -> Text
readmeFileContent = Text.unlines . \case
    "command-wrapper" ->
        [ "# Command Wrapper configuration"
        , ""
        , "Tool for creating customised command-line toolsets.  This directory\
            \ contains"
        , "its top-level configuration"
        , ""
        , ""
        , "## Documentation"
        , ""
        , "Offline documentation is provided in the form of manual pages.\
            \  Best starting"
        , "point is `command-wrapper(1)`."
        , ""
        , "Online documentation is available on\
            \ [github.com/trskop/command-wrapper"
        , "](https://github.com/trskop/command-wrapper)."
        ]

    toolsetName ->
        [ "# Configuration for Command Wrapper toolset "
            <> fromString toolsetName
        , ""
        , "Custom toolset built using Command Wrapper."
        ]

checkFile :: FilePath -> IO (Either FilePath FilePath)
checkFile file = do
    doesExist <- doesFileExist file
    pure if doesExist
        then Right file
        else Left file

data ConfigFile
    = DefaultConfig String FilePath FilePath
    | CommonAliasesConfig String
    | CommonHelpTxt String
    | CdConfig String
    | CommonDirectoriesConfig String
    | ExecLibrary
    | ExecConfig String
    | CommonCommandsConfig String
    | SkelConfig String
    | Library

-- TODO: It would be best to embed these values using 'staticDhallExpression',
-- however, that would strip away comments.
configFileContent :: ConfigFile -> Text
configFileContent = Text.unlines . \case
    DefaultConfig "command-wrapper" libDir manDir ->
        [ "let CommandWrapper = ./library.dhall"
        , ""
        , "let emptyAliases = CommandWrapper.ToolsetConfig.emptyAliases"
        , ""
        , "let aliases"
        , "      : List CommandWrapper.SubcommandAlias.Type"
        , "      =   ./default/aliases-common.dhall"
        , "        # (./default/aliases-local.dhall ? emptyAliases)"
        , "        # (./default/aliases.dhall ? emptyAliases)"
        , ""
        , "let helpMessage"
        , "      : Text"
        , "      =     (./default/help-common.txt as Text)"
        , "        ++  (./default/help-local.txt as Text ? \"\")"
        , "        ++  (./default/help.txt as Text ? \"\")"
        , ""
        , "in  CommandWrapper.ToolsetConfig::{"
        , "    , aliases = aliases"
        , ""
        , "    -- Extra help message is printed at the bottom of help message."
        , "    , extraHelpMessage = Some helpMessage"
        , ""
        , "    -- Path where Command Wrapper will search for external\
            \ subcommands.  If"
        , "    -- specific toolset has set 'searchPath' as well then that will"
        , "    -- be prepended to this one."
        , "    , searchPath = [" <> fromString (show libDir) <> "]"
        , ""
        , "    -- Path where Command Wrapper will search for manual pages.  If"
        , "    -- specific toolset has set 'manPath' as well then that will be"
        , "    -- appended to this one."
        , "    , manPath = [" <> fromString (show manDir) <> "]"
        , "    }"
        ]

    DefaultConfig _ libDir manDir ->
        [ "let CommandWrapper = ../command-wrapper/library.dhall"
        , ""
        , "let emptyAliases = CommandWrapper.ToolsetConfig.emptyAliases"
        , ""
        , "let aliases"
        , "      : List CommandWrapper.SubcommandAlias.Type"
        , "      =   ./default/aliases-common.dhall"
        , "        # (./default/aliases-local.dhall ? emptyAliases)"
        , "        # (./default/aliases.dhall ? emptyAliases)"
        , ""
        , "let helpMessage"
        , "      : Text"
        , "      =     (./default/help-common.txt as Text)"
        , "        ++  (./default/help-local.txt as Text ? \"\")"
        , "        ++  (./default/help.txt as Text ? \"\")"
        , ""
        , "in  CommandWrapper.ToolsetConfig::{"
        , "    , aliases = aliases"
        , ""
        , "    -- Toolset description printed as a header of a help message."
        , "    , description ="
        , "        Some \"TODO: I promise to describe this toolset one day.\""
        , ""
        , "    -- Extra help message is printed at the bottom of help message."
        , "    , extraHelpMessage = Some helpMessage"
        , ""
        , "    -- Path where this toolset will search for its external\
            \ subcommands."
        , "    , searchPath = [" <> fromString (show libDir) <> "]"
        , ""
        , "    -- Path where this toolset will search for its manual pages."
        , "    , manPath = [" <> fromString (show manDir) <> "]"
        , "    }"
        ]

    CommonAliasesConfig "command-wrapper" ->
        [ "-- This file is intended to be under version control and shared\
            \ among multiple"
        , "-- systems. It defines aliases that should be available everywhere\
            \ and via all"
        , "-- toolsets."
        , "--"
        , "-- Aliases that are ment to be available only on this specific\
            \ machine should"
        , "-- go into `./aliases-local.dhall`.  If local configuration is\
            \ under version"
        , "-- control then `./aliases-local.dhall` should be a symbolic link\
            \ to that"
        , "-- version controlled file, or it should contain an import of such\
            \ file.  There"
        , "-- is also `./aliases.dhall` which is intended to be used as a kind\
            \ of staging"
        , "-- environment, and it should not be under version control."
        , ""
        , "  [ { alias = \"h\""
        , "    , description = Some \"Short hand for \\\"help\\\".\""
        , "    , command = \"help\""
        , "    , arguments = [] : List Text"
        , "    }"
        , ""
        , "  , { alias = \"man\""
        , "    , description = Some \"Short hand for \\\"help --man\\\".\""
        , "    , command = \"help\""
        , "    , arguments = [\"--man\"]"
        , "    }"
        , ""
        , "  -- The advantage of having `cfg` as an alias for `config` is that\
            \ it shares"
        , "  -- only one letter of its prefix with `completion`, which is\
            \ useful when"
        , "  -- using command line completion."
        , "  , { alias = \"cfg\""
        , "    , description = Some \"Short hand for \\\"config\\\".\""
        , "    , command = \"config\""
        , "    , arguments = [] : List Text"
        , "    }"
        , ""
        , "  , { alias = \"dhall\""
        , "    , description = Some \"Short hand for \\\"config --dhall\\\".\""
        , "    , command = \"config\""
        , "    , arguments = [\"--dhall\"]"
        , "    }"
        , ""
        , "  , { alias = \"dhall-repl\""
        , "    , description = Some\
            \ \"Short hand for \\\"config --dhall-repl\\\".\""
        , "    , command = \"config\""
        , "    , arguments = [\"--dhall-repl\"]"
        , "    }"
        , "  ]"
        , ": List"
        , "    { alias : Text"
        , "    , description : Optional Text"
        , "    , command : Text"
        , "    , arguments : List Text"
        , "    }"
        ]

    CommonAliasesConfig _ ->
        [ "-- This file is intended to be under version control and shared\
            \ among multiple"
        , "-- systems."
        , "--"
        , "-- Aliases that are ment to be available only on this specific\
            \ machine should"
        , "-- go into `./aliases-local.dhall`.  If local configuration is\
            \ under version"
        , "-- control then `./aliases-local.dhall` should be a symbolic link\
            \ to that"
        , "-- version controlled file, or it should contain an import of such\
            \ file.  There"
        , "-- is also `./aliases.dhall` which is intended to be used as a kind\
            \ of staging"
        , "-- environment, and it should not be under version control."
        , ""
        , "  [ -- { alias = \"something\""
        , "    -- , description = None Text"
        , "    -- , command = \"some-subcommand\""
        , "    -- , arguments = [] : List Text"
        , "    -- }"
        , "  ]"
        , ": List"
        , "    { alias : Text"
        , "    , description : Optional Text"
        , "    , command : Text"
        , "    , arguments : List Text"
        , "    }"
        ]

    CdConfig "command-wrapper" ->
        [ "let CommandWrapper = ./library.dhall"
        , ""
        , "let emptyDirectories = CommandWrapper.CdConfig.emptyDirectories"
        , ""
        , "let directories"
        , "      : List Text"
        , "      =   ./cd/directories-common.dhall"
        , "        # (./cd/directories-local.dhall ? emptyDirectories)"
        , "        # (./cd/directories.dhall ? emptyDirectories)"
        , ""
        , "in  CommandWrapper.CdConfig::{"
        , "    , directories = directories"
        , "    , menuTool ="
        , "          λ(query : Optional Text)"
        , "        → let fzf = CommandWrapper.CdConfig.menu-tool.fzf query"
        , "          in  fzf //  { arguments = [\"--height=40%\"]\
            \ # fzf.arguments }"
        , ""
        , "    -- Here we can set what terminal emulator should be executed.  Some"
        , "    -- definitions are already available in Command Wrapper library list"
        , "    -- them one can use Dhall interpreter `TOOLSET config --dhall` where"
        , "    -- following expression can be evaluated:"
        , "    --"
        , "    -- ```"
        , "    -- (~/.config/command-wrapper/library.dhall).TerminalEmulator"
        , "    -- ```"
        , "--  , terminalEmulator = CommandWrapper.CdConfig.defaul.terminalEmulator"
        , "    }"
        ]

    CdConfig _ ->
        [ "let global = ../command-wrapper/command-wrapper-cd.dhall"
        , ""
        , "let empty = [] : List Text"
        , ""
        , "in      global"
        , "    //  { directories ="
        , "              global.directories"
        , "            # ./cd/directories-common.dhall"
        , "            # (./cd/directories.dhall ? empty)"
        , "            # (./cd/directories-local.dhall ? empty)"
        , "        }"
        ]

    CommonDirectoriesConfig "command-wrapper" ->
        [ "let home = env:HOME as Text"
        , ""
        , "let config = env:XDG_CONFIG_HOME as Text ? \"${home}/.config\""
        , ""
        , "let local = \"${home}/.local\""
        , ""
        , "in    [ \"${config}\""
        , "      , \"${config}/command-wrapper\""
        , "      , \"${local}/lib/command-wrapper\""
        , "      , \"${home}/Downloads\""
        , "      , \"${home}/.ssh\""
        , "      ]"
        , "    : List Text"
        ]

    CommonDirectoriesConfig toolsetName ->
        [ "let home = env:HOME as Text"
        , ""
        , "let config = env:XDG_CONFIG_HOME as Text ? \"${home}/.config\""
        , ""
        , "let local = \"${home}/.local\""
        , ""
        , "in    [ \"${config}/" <> fromString toolsetName <> "\""
        , "      , \"${local}/lib/" <> fromString toolsetName <> "\""
        , "      ]"
        , "    : List Text"
        ]

    CommonHelpTxt "command-wrapper" ->
        [ ""
        , "Global Subcommands:"
        , ""
        , "  help       (internal, aliases: h, man)"
        , "  config     (internal, aliases: cfg, dhall, dhall-repl)"
        , "  version    (internal)"
        , "  completion (internal)"
        , "  cd         (external)"
        , "  exec       (external)"
        , "  skel       (external)"
        ]

    CommonHelpTxt _ ->
        [ ""
        , "TODO: Custom help message, please, edit `help-common.txt`."
        ]

    -- ${CONFIG_DIR}/command-wrapper/exec/library.dhall
    ExecLibrary ->
        [ "https://raw.githubusercontent.com/trskop/command-wrapper/master/dhall/Exec/package.dhall"
        ]

    -- ${CONFIG_DIR}/command-wrapper/command-wrapper-exec.dhall
    ExecConfig "command-wrapper" ->
        [ "let CommandWrapper = ./library.dhall"
        , ""
        , "let empty = [] : List CommandWrapper.ExecNamedCommand"
        , ""
        , "in  { commands ="
        , "          ./exec/commands-common.dhall"
        , "        # (./exec/commands.dhall ? empty)"
        , "        # (./exec/commands-local.dhall ? empty)"
        , "    }"
        ]

    -- ${CONFIG_DIR}/${toolset}/command-wrapper-exec.dhall
    ExecConfig _ ->
        [ "let CommandWrapper = ../command-wrapper/library.dhall"
        , ""
        , "let global = ../command-wrapper/command-wrapper-exec.dhall"
        , ""
        , "let empty = [] : List CommandWrapper.ExecNamedCommand.Type"
        , ""
        , "in    global"
        , "    //  { commands ="
        , "              global.commands"
        , "            # ./exec/commands-common.dhall"
        , "            # (./exec/commands.dhall ? empty)"
        , "            # (./exec/commands-local.dhall ? empty)"
        , "        }"
        ]

    CommonCommandsConfig "command-wrapper" ->
        [ "let CommandWrapper = ../library.dhall"
        , ""
        , "let named = CommandWrapper.ExecNamedCommand.namedCommand"
        , ""
        , "let noExtraEnvironment = CommandWrapper.Command.emptyEnvironment"
        , ""
        , "in"
        , "      [{- named \"echo\""
        , "            ( λ(verbosity : CommandWrapper.Verbosity.Type)"
        , "              → λ(colourOutput : CommandWrapper.ColourOutput.Type)"
        , "              → λ(arguments : List Text)"
        , "              → { command = \"echo\""
        , "                , arguments = arguments"
        , "                , environment = noExtraEnvironment"
        , "                , searchPath = True"
        , "                , workingDirectory = None Text"
        , "                }"
        , "              )"
        , "        //  { description ="
        , "                Some \"TODO: I hereby promise to describe this command.\""
        , "            }"
        , "      -}"
        , "      ]"
        , "    : List CommandWrapper.ExecNamedCommand.Type"
        ]

    CommonCommandsConfig _ ->
        [ "let CommandWrapper = ../../command-wrapper/library.dhall"
        , ""
        , "let named = CommandWrapper.ExecNamedCommand.namedCommand"
        , ""
        , "let noExtraEnvironment = CommandWrapper.Command.emptyEnvironment"
        , ""
        , "in"
        , "      [{- named \"echo\""
        , "            ( λ(verbosity : CommandWrapper.Verbosity.Type)"
        , "              → λ(colourOutput : CommandWrapper.ColourOutput.Type)"
        , "              → λ(arguments : List Text)"
        , "              → { command = \"echo\""
        , "                , arguments = arguments"
        , "                , environment = noExtraEnvironment"
        , "                , searchPath = True"
        , "                , workingDirectory = None Text"
        , "                }"
        , "              )"
        , "        //  { description ="
        , "                Some \"TODO: I hereby promise to describe this command.\""
        , "            }"
        , "      -}"
        , "      ]"
        , "    : List CommandWrapper.ExecNamedCommand.Type"
        ]

    SkelConfig "command-wrapper" ->
        [ "let CommandWrapper = ./library.dhall"
        , ""
        , "let home = env:HOME as Text"
        , ""
        , "let config = env:XDG_CONFIG_HOME as Text ? \"${home}/.config\""
        , ""
        , "let lib = \"${home}/.local/lib\""
        , ""
        , "in    λ(toolset : Text)"
        , "    → λ(subcommand : Text)"
        , "    → λ(command : Text)"
        , "    →   CommandWrapper.SkelConfig::{"
        , "        , template ="
        , "              λ(language : CommandWrapper.SkelConfig.SkelLanguage)"
        , "            → merge"
        , "              { Haskell ="
        , "                  { targetFile ="
        , "                      \"${config}/${toolset}/toolset/app-${command}/Main.hs\""
        , "                  , executable = False"
        , "                  , template ="
        , "                        ./haskell-skel.dhall"
        , "                      ? CommandWrapper.SkelConfig.template.haskell"
        , "                  }"
        , "              , Bash ="
        , "                  { targetFile = \"${lib}/${toolset}/${command}\""
        , "                  , executable = True"
        , "                  , template ="
        , "                        ./bash-skel.dhall"
        , "                      ? CommandWrapper.SkelConfig.template.bash"
        , "                  }"
        , "              , Dhall ="
        , "                  { targetFile = \"${config}/${toolset}/${command}.dhall\""
        , "                  , executable = False"
        , "                  , template ="
        , "                      (   ./dhall-skel.dhall"
        , "                        ? CommandWrapper.SkelConfig.template.dhall"
        , "                      )"
        , "                  }"
        , "              }"
        , "              language"
        , "        }"
        , "      : CommandWrapper.SkelConfig.Type"
        ]

    SkelConfig _ ->
        [ "let mkGlobal = ../command-wrapper/command-wrapper-skel.dhall"
        , ""
        , "in    λ(toolset : Text)"
        , "    → λ(subcommand : Text)"
        , "    → λ(command : Text)"
        , "    →   let global = mkGlobal toolset subcommand command"
        , "        in      global"
        , "            //  {=}"
        ]

    Library ->
        [ "https://raw.githubusercontent.com/trskop/command-wrapper/master/dhall/CommandWrapper/package.dhall"
        ]
