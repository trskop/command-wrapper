let CommandWrapper = ./Types.dhall

let command = ./command.dhall

let noOptions = [] : List Text

let noExtraEnvironment = [] : List CommandWrapper.EnvironmentVariable

let directoryOption =
        λ(directory : Optional Text)
      → λ(mkDirectoryOption : Text → List Text)
      → Optional/fold Text directory (List Text) mkDirectoryOption noOptions

let executeCommandOptions =
        λ(cmd : Optional CommandWrapper.Command)
      → λ(mkCmdOptions : CommandWrapper.Command → List Text)
      → Optional/fold CommandWrapper.Command cmd (List Text) mkCmdOptions
          noOptions

let withOptions =
        λ(termCmd : Text)
      → λ(mkDirectoryOption : Text → List Text)
      → λ(mkCmdOptions : CommandWrapper.Command → List Text)
      → λ(directory : Optional Text)
      → λ(cmd : Optional CommandWrapper.Command)
      →   command.withExtraArguments (command.simple termCmd)
            (  directoryOption directory mkDirectoryOption
            # executeCommandOptions cmd mkCmdOptions
            )
        ∧ {environment = noExtraEnvironment}
      : CommandWrapper.CommandWithEnvironment

let -- These are some known terminal emulators that support setting working
    -- directory via command line option.
    terminalEmulators =
      { kitty =
          withOptions "kitty"
            (λ(directory : Text) → ["--directory", directory])
            ( λ(c : CommandWrapper.Command)
            → ["--", c.command] # c.arguments
            )

      , urxvt =
          withOptions "urxvt"
            (λ(directory : Text) → ["-cd", directory])
            ( λ(c : CommandWrapper.Command)
            → ["-e", c.command] # c.arguments
            )
      }

in  terminalEmulators
      : { urxvt : CommandWrapper.TerminalEmulator
        , kitty : CommandWrapper.TerminalEmulator
        }
