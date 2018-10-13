  let
    Default = ./Default.dhall

in let
    Cd = ./Cd.dhall

in let
    Skel = ./Skel.dhall

in let
    Exec = ./Exec.dhall

in let
    CommandWrapper =
      { Command = ./Command.dhall
      , SubcommandAlias = ./SubcommandAlias.dhall
      , TerminalEmulator = ./TerminalEmulator.dhall
      , Verbosity = ./Verbosity.dhall
      , ColourOutput = ./ColourOutput.dhall

      -- Toolset default (main/global) configuration
      , DefaultConfig = Default.Config
      , DefaultMkConfig = Default.MkConfig

      -- Configuration for `skel` subcommand
      , SkelTemplate = Skel.Template
      , SkelLanguageTemplates = Skel.LanguageTemplates
      , SkelConfig = Skel.Config
      , SkelMkLangageTemplates = Skel.MkLangageTemplates
      , SkelMkConfig = Skel.MkConfig

      -- Configuration for `cd` subcommand
      , CdConfig = Cd.Config
      , CdMkConfig = Cd.MkConfig

      -- Configuration for `exec` subcommand
      , EnvironmentVariable = Exec.EnvironmentVariable
      , ExecCommand = Exec.Command
      , ExecNamedCommand = Exec.NamedCommand
      }
in  CommandWrapper
