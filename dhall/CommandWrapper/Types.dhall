-- TODO: Rename 'Default' to 'Toolset'?
let Default = ./Type/Default.dhall

let Cd = ./Type/Cd.dhall

let Skel = ./Type/Skel.dhall

let Exec = ./Type/Exec.dhall

in  { Command = ./Type/Command.dhall
    , CommandWithEnvironment = ./Type/CommandWithEnvironment.dhall
    , EnvironmentVariable = ./Type/EnvironmentVariable.dhall
    , SubcommandAlias = ./Type/SubcommandAlias.dhall
    , TerminalEmulator = ./Type/TerminalEmulator.dhall
    , Verbosity = ./Type/Verbosity.dhall
    , ColourOutput = ./Type/ColourOutput.dhall

    -- Toolset default (main/global) configuration
    , DefaultConfig = Default.Config
    , DefaultMkConfig = Default.MkConfig

    -- Configuration for `skel` subcommand
    , SkelLanguage = Skel.Language
    , SkelTemplate = Skel.Template
    , SkelConfig = Skel.Config
    , SkelMkConfig = Skel.MkConfig

    -- Configuration for `cd` subcommand
    , CdConfig = Cd.Config
    , CdMkConfig = Cd.MkConfig

    -- Configuration for `exec` subcommand
    , ExecCommand = Exec.Command
    , ExecNamedCommand = Exec.NamedCommand
    }
