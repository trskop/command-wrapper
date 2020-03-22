-- |
-- Module:       $Header$
-- Description:  Top-level executable of Command Wrapper.
-- Copyright:    (c) 2014-2020 Peter Trsko
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  GHC specific language extensions.
--
-- Top-level executable of Command Wrapper, it's usually installed as
-- @${HOME}/.local/lib/command-wrapper/command-wrapper@.
module CommandWrapper.Toolset.Main (main)
  where

import Control.Applicative ((<*>), (<|>), many, pure)
import Control.Exception
    ( SomeAsyncException
    , SomeException
    , catch
    , displayException
    , fromException
    , throwIO
    )
import Control.Monad ((>>=))
import Data.Bool (Bool(False, True))
import Data.Either (Either(Left, Right), either)
import Data.Eq ((/=))
import Data.Foldable (for_)
import Data.Function (($), (.), id)
import Data.Functor ((<$>), (<&>), fmap)
import Data.Maybe (Maybe(Just, Nothing), catMaybes, fromMaybe, maybe)
import Data.Monoid (Endo(Endo, appEndo), mconcat, mempty)
import Data.Semigroup ((<>))
import Data.String (String, fromString)
import Data.Traversable (forM)
import System.Exit (ExitCode(ExitFailure), die, exitWith)
import System.IO (FilePath, IO, stderr)
import Text.Show (show)

import Data.Monoid.Endo.Fold (dualFoldEndo)
import qualified Data.Text as Text (unpack)
import Data.Text.Prettyprint.Doc ((<+>))
import qualified Data.Text.Prettyprint.Doc as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Pretty (AnsiStyle)
import qualified Data.Text.Prettyprint.Doc.Util as Pretty (reflow)
import qualified Mainplate.Extensible as Mainplate
    ( Command(Internal)
    , runExtensibleAppWith
    )
import qualified Options.Applicative as Options
import qualified Options.Applicative.Standard as Options
import System.Directory (doesFileExist, setCurrentDirectory)
import System.FilePath ((</>), splitSearchPath)

import CommandWrapper.Core.Environment
    ( AppNameError(..)
    , AppNames(..)
    , CommandWrapperPrefix
    , CommandWrapperToolsetVarName
        ( CommandWrapperLocalConfigDir
        , CommandWrapperManPath
        , CommandWrapperPath
        , CommandWrapperUserConfigDir
        )
    , ParseEnv
    , commandWrapperToolsetVarName
    , defaultCommandWrapperPrefix
    , getAppNames
    , optionalVar
    , parseEnvIO
    , var
    )
import CommandWrapper.Core.Config.ColourOutput (ColourOutput)
import qualified CommandWrapper.Core.Config.ColourOutput as ColourOutput
    ( ColourOutput(Auto)
    , options
    , noColorEnvVar
    )
import CommandWrapper.Core.Config.Verbosity (Verbosity(Silent), setVerbosity)
import qualified CommandWrapper.Core.Help.Pretty as Help
    ( dullGreen
    , helpOptions
    , longOption
    , metavar
    , optionDescription
    , optionalMetavar
    , optionalSubcommand
    , section
    , shortOption
    , subcommand
    , usageSection
    , value
    )
import CommandWrapper.Core.Message (Result(..), errorMsg)
import qualified CommandWrapper.Toolset.Config.Global as Global
    ( Config(Config)
    , ConfigPaths(ConfigPaths)
    , defConfigPaths
    )
import qualified CommandWrapper.Toolset.Config.Global as Global.Config
    ( Config(..)
    , ConfigPaths(..)
    , ManPath(..)
    , SearchPath(..)
    , def
    , getAliases
    )
import qualified CommandWrapper.Toolset.Config.File as Config.File (apply, read)
import qualified CommandWrapper.Toolset.ExternalSubcommand as External
import qualified CommandWrapper.Toolset.InternalSubcommand as Internal
import qualified CommandWrapper.Toolset.Main.StaticConfig as StaticConfig
import CommandWrapper.Toolset.Main.StaticConfig (StaticConfig(StaticConfig))
import qualified CommandWrapper.Toolset.Options as Options

import Paths_command_wrapper (version)


readConfig :: Options.Command a -> IO (Either String (Endo Global.Config))
readConfig _ = pure (Right mempty)

defaults
    :: Global.Config
    -> Endo (Options.Command Global.Config)
    -> IO (Options.Command Global.Config)
defaults config (Endo f) =
    pure . f $ Mainplate.Internal (Internal.HelpCommand []) config

main :: StaticConfig CommandWrapperPrefix -> IO ()
main staticConfig = do
    appNames@AppNames{exeName, usedName} <- getAppNames'
    -- TODO: It would be great to have debugging message with 'appNames' in it.
    defaultConfig <- parseEnv staticConfig

    config <- (`appEndo` defaultConfig) <$> do
        let Global.Config
                { configPaths = Global.ConfigPaths{system, user, local}
                }
                = defaultConfig

            -- Will always be non-empty directory, but the ordering makes it
            -- cumbersome to use NonEmpty list.
            configDirs = catMaybes [system, Just user, local]

        dualFoldEndo
            <$> forM configDirs (readGlobalConfig exeName)
            <*> ( if exeName /= usedName
                     then forM configDirs (readGlobalConfig usedName)
                     else pure mempty
                )

    Mainplate.runExtensibleAppWith (parseOptions appNames config) readConfig
        (defaults config) (go External.run appNames)
        (go (Internal.run helpMessage) appNames)
  where
    readGlobalConfig baseName dir = do
        let configFile = dir </> baseName </> "default.dhall"
        configExists <- doesFileExist configFile
        -- TODO: Debugging message that tells the user that we either read the
        --   configuration file or that we are using hardcoded defaults.
--      print (if configExists then "Reading" else "No such file", configFile)
        if configExists
            then
                Config.File.read configFile
                    >>= either die (pure . Config.File.apply)
            else
                pure id

    go  :: (AppNames -> command -> Global.Config -> IO ())
        -> AppNames
        -> command
        -> Global.Config
        -> IO ()
    go run appNames@AppNames{usedName} command config@Global.Config{..} = do
        -- TODO: Nice error message when we fail to set the working directory.
        for_ changeDirectory \dir ->
            setCurrentDirectory dir `catch` \(e :: SomeException) ->
                case fromException @SomeAsyncException e of
                    Just _ -> throwIO e  -- Not touching this!
                    Nothing -> do
                        errorMsg (fromString usedName) verbosity colourOutput
                            stderr (fromString (displayException e) <> ".")
                        exitWith (ExitFailure 1)

        run appNames command config

getAppNames' :: IO AppNames
getAppNames' = getAppNames defaultCommandWrapperPrefix (pure version) >>= \case
    Left RunningInInteractiveInterpreterError ->
        die ( "command-wrapper: Unable to resolve its executable path in\
            \ GHCi.  Consider setting " <> facadeEnvVarName
            <> " environment variable to bypass this restriction."
            )

    Left (FacadeDoesNotExistOrIsNotAFileError path) ->
        die ( "command-wrapper: " <> facadeEnvVarName <> ": " <> show path
            <> ": File does not exist or is not a file (e.g. directory)."
            )

    Left (FacadeIsNotExecutableError path) ->
        die ( "command-wrapper: " <> show facadeEnvVarName <> ": " <> show path
            <> ": File is not executable."
            )

    Right appNames ->
        pure appNames
  where
    facadeEnvVarName = Text.unpack (defaultCommandWrapperPrefix <> "_FACADE")

-- | Parse environment variables and produce default configuration that can be
-- later updated by configuration file and command line options.
parseEnv :: StaticConfig CommandWrapperPrefix -> IO Global.Config
parseEnv StaticConfig{..} = parseEnvIO (die . show) do
    defColour <- fromMaybe ColourOutput.Auto <$> ColourOutput.noColorEnvVar

    searchPath <- searchPathVar Global.Config.SearchPath CommandWrapperPath
    manPath    <- searchPathVar Global.Config.ManPath    CommandWrapperManPath

    configPaths <- do
        system <- lookupSystemConfigDir
        user <- lookupUserConfigDir
        local <- lookupLocalConfigDir
        pure (Global.defConfigPaths user)
            { Global.Config.system
            , Global.Config.local
            }

    pure (Global.Config.def defColour searchPath manPath configPaths)
        { Global.Config.searchSystemPath
        }
  where
    searchPathVar
        :: ([FilePath] -> a)
        -> CommandWrapperToolsetVarName
        -> ParseEnv CommandWrapperPrefix a
    searchPathVar mk varName = do
        varName' <- commandWrapperToolsetVarName varName
        mk . maybe [] splitSearchPath <$> optionalVar' varName'

    lookupUserConfigDir :: ParseEnv CommandWrapperPrefix FilePath
    lookupUserConfigDir =
        commandWrapperToolsetVarName CommandWrapperUserConfigDir
        >>= optionalVar' >>= \case
            Just dir ->
                pure dir
            Nothing -> do
                -- This resolution algorithm won't be of much use on
                -- non-Linux/UNIX systems.
                optionalVar' "XDG_CONFIG_HOME" >>= \case
                    Just dir ->
                        pure dir
                    Nothing ->
                        var "HOME" <&> \home ->
                            Text.unpack home </> ".config"

    lookupLocalConfigDir :: ParseEnv CommandWrapperPrefix (Maybe FilePath)
    lookupLocalConfigDir =
        commandWrapperToolsetVarName CommandWrapperLocalConfigDir
        >>= optionalVar'

    optionalVar' varName' = fmap Text.unpack <$> optionalVar varName'

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
            <*> many changeDirectoryOption
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
        Endo \config -> config{Global.Config.ignoreAliases = False}

noAliasesFlag :: Options.Parser (Endo Global.Config)
noAliasesFlag = Options.flag' modifyConfig $ mconcat
    [ Options.long "no-aliases"
    , Options.help "Ignore SUBCOMMAND aliases, dual to '--aliases'."
    ]
  where
    modifyConfig :: Endo Global.Config
    modifyConfig =
        Endo \config -> config{Global.Config.ignoreAliases = True}

changeDirectoryOption :: Options.Parser (Endo Global.Config)
changeDirectoryOption = fmap modifyConfig . Options.strOption $ mconcat
    [ Options.long "change-directory"
    , Options.metavar "DIRECTORY"
    , Options.help "Change current working directory to DIRECTORY."
    ]
  where
    modifyConfig :: FilePath -> Endo Global.Config
    modifyConfig dir =
        Endo \config -> config{Global.Config.changeDirectory = pure dir}

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

helpMessage :: AppNames -> Global.Config -> Pretty.Doc (Result Pretty.AnsiStyle)
helpMessage AppNames{usedName} Global.Config{description} = Pretty.vsep
    [ Pretty.reflow (maybe defaultDescription fromString description)
    , ""

    , Help.usageSection usedName
        [ Help.subcommand <+> subcommandArguments

        , "help"
            <+> Help.optionalMetavar "HELP_OPTIONS"
            <+> Help.optionalSubcommand

        , "config"
            <+> Help.optionalMetavar "CONFIG_OPTIONS"
            <+> Help.optionalSubcommand

        , "version" <+> Help.optionalMetavar "VERSION_OPTIONS"
        , "completion" <+> Help.optionalMetavar "COMPLETION_OPTIONS"

        , Pretty.braces
            (Help.longOption "version" <> "|" <> Help.shortOption 'V')

        , Help.helpOptions
        ]

    , Help.section (Pretty.annotate Help.dullGreen "GLOBAL_OPTIONS" <> ":")
        [ Help.optionDescription ["-v"]
            [ Pretty.reflow "Increment verbosity by one level. Can be used"
            , Pretty.reflow "multiple times."
            ]

        , Help.optionDescription ["--verbosity=VERBOSITY"]
            [ Pretty.reflow "Set verbosity level to"
            , Help.metavar "VERBOSITY" <> "."
            , Pretty.reflow "Possible values of"
            , Help.metavar "VERBOSITY", "are"
            , Pretty.squotes (Help.value "silent") <> ","
            , Pretty.squotes (Help.value "normal") <> ","
            , Pretty.squotes (Help.value "verbose") <> ","
            , "and"
            , Pretty.squotes (Help.value "annoying") <> "."
            ]

        , Help.optionDescription ["--silent", "-s"]
            (silentDescription "quiet")

        , Help.optionDescription ["--quiet", "-q"]
            (silentDescription "silent")

        , Help.optionDescription ["--colo[u]r=WHEN"]
            [ "Set", Help.metavar "WHEN"
            , Pretty.reflow "colourised output should be produced. Possible"
            , Pretty.reflow "values of"
            , Help.metavar "WHEN", "are"
            , Pretty.squotes (Help.value "always") <> ","
            , Pretty.squotes (Help.value "auto") <> ","
            , "and", Pretty.squotes (Help.value "never") <> "."
            ]

        , Help.optionDescription ["--no-colo[u]r"]
            [ Pretty.reflow "Same as"
            , Pretty.squotes (Help.longOption "colour=never") <> "."
            ]

        , Help.optionDescription ["--[no-]aliases"]
            [ "Apply or ignore", Help.metavar "SUBCOMMAND", "aliases."
            , Pretty.reflow  "This is useful when used from e.g. scripts to\
                \ avoid issues with user defined aliases interfering with how\
                \ the script behaves."
            ]

        , Help.optionDescription ["--change-directory=DIRECTORY"]
            [ Pretty.reflow "Change current working directory to"
            , Help.metavar "DIRECTORY"
            , Pretty.reflow "before doing anything.  Internal and external\
                \ subcommands are always executed after changing working\
                \ directory."
            ]

        , Help.optionDescription ["--version", "-V"]
            [ Pretty.reflow "Print version information to stdout and exit."
            ]

        , Help.optionDescription ["--help", "-h"]
            [ Pretty.reflow "Print this help and exit."
            ]
        ]

    , Help.section (Help.metavar "SUBCOMMAND" <> ":")
        [ Pretty.reflow "Name of either internal or external subcommand."
        ]
    , ""
    ]
  where
    silentDescription altOpt =
        [ Pretty.reflow
            "Silent mode. Suppress normal diagnostic or result output. Same as"
        , Pretty.squotes (Help.longOption altOpt) <> ",", "and"
        , Pretty.squotes (Help.longOption "verbosity=silent") <> "."
        ]

    defaultDescription = "Toolset of commands for a working developer."

    subcommandArguments =
        Pretty.brackets (Pretty.annotate Help.dullGreen "SUBCOMMAND_ARGUMENTS")
