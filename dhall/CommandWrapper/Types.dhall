let Skel = ./Type/Skel

in  { Command = ./Type/Command
    , CommandWithEnvironment = ./Type/CommandWithEnvironment
    , ConnectToDatabase = ./Type/ConnectToDatabase
    , EnvironmentVariable = ./Type/EnvironmentVariable
    , SubcommandAlias = ./Type/SubcommandAlias
    , TerminalEmulator = ./Type/TerminalEmulator
    , Verbosity = ./Type/Verbosity
    , ColourOutput = ./Type/ColourOutput
    , Schema = ./Type/Schema
    , Shell = ./Type/Shell
    , Editor = ./Type/Editor

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
    }
