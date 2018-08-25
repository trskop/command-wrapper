  let
    CommandWrapper = ./Type/package.dhall

in
  { simple =
        λ(command : Text)
      → { command = command
        , arguments = [] : List Text
        } : CommandWrapper.Command

  , withExtraArguments =
        λ(command : CommandWrapper.Command)
      → λ(arguments : List Text)
      → { command = command.command
        , arguments = command.arguments # arguments
        } : CommandWrapper.Command
  }
