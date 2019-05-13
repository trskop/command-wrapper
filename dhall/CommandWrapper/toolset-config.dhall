let CommandWrapper = ./Types.dhall

let colourOutput = ./colour-output.dhall

let mkToolsetConfig =
        λ ( context
              : { home : Text
                , toolset : Text
                }
          )
      → λ ( update
              :   ∀(defaults : CommandWrapper.DefaultConfig)
                → CommandWrapper.DefaultConfig
          )
      → update
          { aliases = [] : List CommandWrapper.SubcommandAlias

          , searchPath =
              [ "${context.home}/.local/lib/${context.toolset}"
              ] : List Text

          , description = None Text
          , extraHelpMessage = None Text

          , verbosity = CommandWrapper.Verbosity.Normal
          , colourOutput = None CommandWrapper.ColourOutput
          }

let mkPrefix =
      λ(optionalText : Optional Text)
      → Optional/fold Text optionalText Text
          (λ(t : Text) → "${t}\n") ""

let addSubcommandAliases
      : ∀(aliases : List CommandWrapper.SubcommandAlias)
      → ∀(helpMessage : Text)
      → ∀(defaults : CommandWrapper.DefaultConfig)
      → CommandWrapper.DefaultConfig
      = λ(aliases : List CommandWrapper.SubcommandAlias)
      → λ(helpMessage : Text)
      → λ(defaults : CommandWrapper.DefaultConfig)
      → { aliases = defaults.aliases # aliases
        , description = defaults.description
        , extraHelpMessage =
            Some "${mkPrefix defaults.extraHelpMessage}${helpMessage}"
        , searchPath = defaults.searchPath
        , verbosity = defaults.verbosity
        , colourOutput = defaults.colourOutput
        }

in  { mkConfig = mkToolsetConfig
    , addSubcommandAliases = addSubcommandAliases
    }
