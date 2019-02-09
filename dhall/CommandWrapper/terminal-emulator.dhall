let CommandWrapper = ./Types.dhall

let command = ./command.dhall

let urxvt = command.simple "urxvt"

let terminalEmulator =
      { urxvt =
          {
          -- Start a new terminal emulator without any other specific
          -- requirements.
            standard =
                urxvt
              ∧ {environment = [] : List CommandWrapper.EnvironmentVariable}

          -- Start a new terminal emulator in a specified directory.
          , inDirectory =
              λ(directory : Text)
              →   command.withExtraArguments urxvt ["-cd", directory]
                ∧ {environment = [] : List CommandWrapper.EnvironmentVariable}
          }
      }

in  terminalEmulator
      : { urxvt : CommandWrapper.TerminalEmulator
        }
