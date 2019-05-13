let CommandWrapper = ./Types.dhall

let colourOutput = ./colour-output.dhall

let helpMessage = ''

Global Subcommands:

  help       (internal, aliases: h, man)
  config     (internal, aliases: cfg)
  version    (internal)
  completion (internal)
  cd         (external)
  exec       (external)
  skel       (external)
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
              , { alias = "man"
                , command = "help"
                , arguments = ["--man"] : List Text
                }
              , { alias = "cfg"
                , command = "config"
                , arguments = [] : List Text
                }
              ] : List CommandWrapper.SubcommandAlias

          , searchPath =
              [ "${context.home}/.local/lib/command-wrapper"
              ] : List Text

          , description = None Text
          , extraHelpMessage = Some helpMessage
          , verbosity = CommandWrapper.Verbosity.Normal {=}
          , colourOutput = None CommandWrapper.ColourOutput
          }

in  mkDefault
      : ∀ ( context
              : { home : Text
                }
          )
      → CommandWrapper.DefaultMkConfig
