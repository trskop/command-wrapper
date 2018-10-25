  let
    CommandWrapper = ./Type/package.dhall

in let
    verbosity = ./verbosity.dhall

in let
    colourOutput = ./colour-output.dhall

in let
    mkToolsetConfig =
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

          , extraHelpMessage = None Text

          , verbosity = verbosity.normal
          , colourOutput = None CommandWrapper.ColourOutput
          }

in let
    mkPrefix =
      λ(optionalText : Optional Text)
      → Optional/fold Text optionalText Text
          (λ(t : Text) → "${t}\n") ""

in let
    addSubcommandAliases
      : ∀(aliases : List CommandWrapper.SubcommandAlias)
      → ∀(helpMessage : Text)
      → ∀(defaults : CommandWrapper.DefaultConfig)
      → CommandWrapper.DefaultConfig
      = λ(aliases : List CommandWrapper.SubcommandAlias)
      → λ(helpMessage : Text)
      → λ(defaults : CommandWrapper.DefaultConfig)
      → { aliases = defaults.aliases # aliases
        , extraHelpMessage =
            [ "${mkPrefix defaults.extraHelpMessage}${helpMessage}"
            ] : Optional Text
        , searchPath = defaults.searchPath
        , verbosity = defaults.verbosity
        , colourOutput = defaults.colourOutput
        }

in  { mkConfig = mkToolsetConfig
    , addSubcommandAliases = addSubcommandAliases
    }
