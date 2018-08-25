   let
    Command = ./Command.dhall
in let
    TerminalEmulator =
      { standard : Command
      , inDirectory : ∀(directory : Text) → Command
      }
in  TerminalEmulator
