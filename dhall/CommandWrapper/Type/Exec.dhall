  let
    Command = ./Command.dhall

in let
    Verbosity = ./Verbosity.dhall

in let
    EnvironmentVariable =
      { name : Text
      , value : Text
      }

in let
    ExecCommand = Command
      //\\  { environment : List EnvironmentVariable
            }

in let
    NamedCommand =
      { name : Text
      , command :
          ∀ (verbosity : Verbosity)
        → ∀ (arguments : List Text)
        → ExecCommand
      }

in
    { Command = ExecCommand : Type
    , NamedCommand = NamedCommand : Type
    , EnvironmentVariable = EnvironmentVariable : Type
    }
