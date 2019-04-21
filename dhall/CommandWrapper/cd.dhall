let CommandWrapper = ./Types.dhall

let terminalEmulator = ./terminal-emulator.dhall

let mkCd =
        λ(update : CommandWrapper.CdConfig → CommandWrapper.CdConfig)
      → update
          { directories = [] : List Text

          -- Consider setting full path instead of just command name.
          , menuTool =
                λ(query : Optional Text)
              → { command = "fzf"
                , arguments =
                      ["--reverse"]
                    # Optional/fold Text query (List Text)
                        (λ(query : Text) → ["--query=${query}"])
                        ([] : List Text)
                , environment = [] : List CommandWrapper.EnvironmentVariable
                } : CommandWrapper.CommandWithEnvironment

          -- System shell will be used by default.
          , shell = None Text

          , terminalEmulator =
                λ(directory : Text)
              → λ(command : Optional CommandWrapper.Command)
              → terminalEmulator.urxvt (Some directory) command : CommandWrapper.CommandWithEnvironment
          }
        : CommandWrapper.CdConfig

in  mkCd : CommandWrapper.CdMkConfig
