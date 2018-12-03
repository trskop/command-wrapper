let CommandWrapper = ./Type/package.dhall

let verbosity = ./verbosity.dhall

let colourOutput = ./colour-output.dhall

let helpMessage = ''

Internal Subcommands:

  help     (aliases: h, hlp)

External Subcommands:

  cd
  exec
  skel
  completion
''

let mkDefault =
        λ ( context
              : { home : Text
                }
          )
      → λ ( update
              :   ∀(default : CommandWrapper.DefaultConfig)
                → CommandWrapper.DefaultConfig
          )
      → update
          { aliases =
              [ { alias = "h"
                , command = "help"
                , arguments = [] : List Text
                }
              , { alias = "hlp"
                , command = "help"
                , arguments = [] : List Text
                }
              ] : List CommandWrapper.SubcommandAlias

          , searchPath =
              [ "${context.home}/.local/lib/command-wrapper"
              ] : List Text

          , extraHelpMessage = Some helpMessage
          , verbosity = verbosity.normal
          , colourOutput = None CommandWrapper.ColourOutput
          }

in  mkDefault
      : ∀ ( context
              : { home : Text
                }
          )
      → CommandWrapper.DefaultMkConfig
