let Skel = ./Type/Skel

in  { ColourOutput = ./ColourOutput/Type
    , Command = ./Command/Type
    , CommandWithEnvironment = ./CommandWithEnvironment/Type
    , ConnectToDatabase = ./ConnectToDatabase/Type
    , Editor = ./Editor/Type
    , EnvironmentVariable = ./EnvironmentVariable/Type
    , ExitCode = ./ExitCode/Type
    , NotifyWhen = ./NotifyWhen/Type
    , Schema = ./Schema/Type
    , Shell = ./Shell/Type
    , SubcommandAlias = ./SubcommandAlias/Type
    , TerminalEmulator = ./TerminalEmulator/Type
    , Verbosity = ./Verbosity/Type

    -- Toolset default (main/global) configuration
    , ToolsetConfig = ./Type/ToolsetConfig

    -- Configuration for `skel` subcommand
    , SkelTemplate = ./Type/SkelTemplate
    , SkelLanguage = Skel.Language
    , SkelConfig = Skel.Config

    -- Configuration for `cd` subcommand
    , CdConfig = ./Type/CdConfig

    -- Configuration for `exec` subcommand
    , ExecCommand = ./ExecCommand/Type
    , ExecNamedCommand = ./ExecNamedCommand/Type

    -- Help message
    , HelpMessage = ./Type/HelpMessage
    , HelpAnnotation = ./Type/HelpAnnotation
    , HelpAnnotated = ./Type/HelpAnnotated
    }
