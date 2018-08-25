  let
    CommandWrapper = ./Type/package.dhall

in let
    verbosity = https://raw.githubusercontent.com/trskop/verbosity/master/dhall/Verbosity/package.dhall

in let
    -- TODO: Move to types
    SubcommandAlias =
      { alias : Text
      , command : Text
      , arguments : List Text
      }

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
              ] : List SubcommandAlias

          , searchPath =
              [ "${context.home}/.local/lib/command-wrapper"
              ] : List Text

          , extraHelpMessage =
              [ "\nInternal Subcommands:\n\n  help     (aliases: h, hlp)\n"
              ] : Optional Text

          , verbosity = verbosity.normal
          }

in  mkDefault
      : ∀ ( context
              : { home : Text
                }
          )
      → CommandWrapper.DefaultMkConfig
