let Skel = ./Type/Skel

in  { ColourOutput = ./ColourOutput/Type
    , Command = ./Command/Type
    , CommandWithEnvironment = ./CommandWithEnvironment/Type
    , ConnectToDatabase = ./Type/ConnectToDatabase
    , Editor = ./Editor/Type
    , EnvironmentVariable = ./EnvironmentVariable/Type
    , ExitCode = ./Type/ExitCode
    , NotifyWhen = ./Type/NotifyWhen
    , Schema = ./Schema/Type
    , Shell = ./Type/Shell
    , SubcommandAlias = ./Type/SubcommandAlias
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
    , ExecCommand = ./Type/ExecCommand
    , ExecNamedCommand = ./Type/ExecNamedCommand

    -- Help message
    , HelpMessage = ./Type/HelpMessage
    , HelpAnnotation = ./Type/HelpAnnotation
    , HelpAnnotated = ./Type/HelpAnnotated
    }
