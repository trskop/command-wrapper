  let
    CommandWrapper = ./Type/package.dhall

in let
    command = ./command.dhall

in let
    urxvt = command.simple "urxvt"

in let
    terminalEmulator =
      { urxvt =
          {
          -- Start a new terminal emulator without any other specific
          -- requirements.
            standard = urxvt

          -- Start a new terminal emulator in a specified directory.
          , inDirectory =
              λ(directory : Text)
              → command.withExtraArguments urxvt ["-cd", directory]
          }
      }

in  terminalEmulator
      : { urxvt : CommandWrapper.TerminalEmulator
        }
