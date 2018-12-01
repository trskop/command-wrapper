let CommandWithEnvironment : Type = ./CommandWithEnvironment.dhall

let Verbosity : Type = ./Verbosity.dhall

let ColourOutput : Type = ./ColourOutput.dhall

let ExecCommand : Type = CommandWithEnvironment
      ⩓ { searchPath : Bool
        , workingDirectory : Optional Text
        }

let NamedCommand : Type =
      { name : Text
      , command :
          ∀ (verbosity : Verbosity)
        → ∀ (colourOutput : ColourOutput)
        → ∀ (arguments : List Text)
        → ExecCommand
      }

in
    { Command = ExecCommand : Type
    , NamedCommand = NamedCommand : Type
    }
