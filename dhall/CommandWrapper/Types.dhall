let Skel = ./Type/Skel

in  { ColourOutput = ./Type/ColourOutput
    , Command = ./Type/Command
    , CommandWithEnvironment = ./Type/CommandWithEnvironment
    , ConnectToDatabase = ./Type/ConnectToDatabase
    , Editor = ./Type/Editor
    , EnvironmentVariable = ./Type/EnvironmentVariable
    , ExitCode = ./Type/ExitCode
    , NotifyWhen = ./Type/NotifyWhen
    , Schema = ./Type/Schema
    , Shell = ./Type/Shell
    , SubcommandAlias = ./Type/SubcommandAlias
    , TerminalEmulator = ./Type/TerminalEmulator
    , Verbosity = ./Type/Verbosity

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
