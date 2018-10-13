  let
    CommandWrapper = ./Type/package.dhall

in let
    verbosity = ./verbosity.dhall

in let
    colourOutput = ./colour-output.dhall

in let
    helpMessage = ''

Internal Subcommands:

  help     (aliases: h, hlp)

External Subcommands:

  cd
  exec
  skel
  completion
''

in let
    mkDefault =
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

          , extraHelpMessage = [ helpMessage ]: Optional Text
          , verbosity = verbosity.normal
          , colourOutput = colourOutput.auto
          }

in  mkDefault
      : ∀ ( context
              : { home : Text
                }
          )
      → CommandWrapper.DefaultMkConfig
