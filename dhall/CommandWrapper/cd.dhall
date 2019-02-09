let CommandWrapper = ./Types.dhall

let terminalEmulator = ./terminal-emulator.dhall

let mkCd =
        λ ( context
              : { home : Text
                }
          )
      → λ ( update
              : ∀(default : CommandWrapper.CdConfig) → CommandWrapper.CdConfig
          )
      → update
          { directories =
              [ "${context.home}/.config"
              , "${context.home}/.local/lib/command-wrapper"
              , "${context.home}/Downloads"
              ] : List Text

          -- Consider setting full path instead of just command name.
          , menuTool =
              { command = "fzf"
              , arguments = [] : List Text
              , environment = [] : List CommandWrapper.EnvironmentVariable
              }

          -- Some systems may not have Bash in default search path. Consider
          -- using absolute file path instead.
          , shell = "bash"

          , terminalEmulator = terminalEmulator.urxvt.inDirectory
          }

in  mkCd
      : ∀ ( context
              : { home : Text
                }
          )
      → CommandWrapper.CdMkConfig
