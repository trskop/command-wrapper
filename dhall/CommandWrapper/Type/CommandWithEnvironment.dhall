let Command : Type = ./Command.dhall

let EnvironmentVariable : Type = ./EnvironmentVariable.dhall

let CommandWithEnvironment : Type =
      Command ⩓ {environment : List EnvironmentVariable}

in CommandWithEnvironment : Type
