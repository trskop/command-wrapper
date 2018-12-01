let CommandWithEnvironment = ./CommandWithEnvironment.dhall

let TerminalEmulator =
      { standard : CommandWithEnvironment
      , inDirectory : ∀(directory : Text) → CommandWithEnvironment
      }

in  TerminalEmulator : Type
