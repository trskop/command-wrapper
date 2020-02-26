{ ColourOutput = ./ColourOutput/Type
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
, ToolsetConfig = ./ToolsetConfig/Type

-- Configuration for `skel` subcommand
, SkelTemplate = ./FileTemplate/Type
, SkelLanguage = ./SkelConfig/SkelLanguage
, SkelConfig = ./SkelConfig/Type

-- Configuration for `cd` subcommand
, CdConfig = ./CdConfig/Type

-- Configuration for `exec` subcommand
, ExecCommand = ./ExecCommand/Type
, ExecNamedCommand = ./ExecNamedCommand/Type

-- Help message
, HelpMessage = ./Type/HelpMessage
, HelpAnnotation = ./Type/HelpAnnotation
, HelpAnnotated = ./Type/HelpAnnotated
}
