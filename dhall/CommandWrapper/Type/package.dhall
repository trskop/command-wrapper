-- TODO: Rename 'Default' to 'Toolset'?
let Default = ./Default.dhall

let Cd = ./Cd.dhall

let Skel = ./Skel.dhall

let Exec = ./Exec.dhall

in  { Command = ./Command.dhall
    , CommandWithEnvironment = ./CommandWithEnvironment.dhall
    , EnvironmentVariable = ./EnvironmentVariable.dhall
    , SubcommandAlias = ./SubcommandAlias.dhall
    , TerminalEmulator = ./TerminalEmulator.dhall
    , Verbosity = ./Verbosity.dhall
    , ColourOutput = ./ColourOutput.dhall

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
