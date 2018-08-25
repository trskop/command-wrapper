  let
    CommandWrapper = ./Type/package.dhall

in let
    terminalEmulator = ./terminal-emulator.dhall

in let
    mkCd =
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
          , menuTool = "fzf"

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
