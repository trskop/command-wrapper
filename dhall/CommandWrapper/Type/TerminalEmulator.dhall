let Command = ./Command.dhall

let CommandWithEnvironment = ./CommandWithEnvironment.dhall

let TerminalEmulator =
        Optional Text
      → Optional Command
      → CommandWithEnvironment

in  TerminalEmulator : Type
