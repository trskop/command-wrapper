  let
    CommandWrapper = ./Type/package.dhall

in let
    verbosity = https://raw.githubusercontent.com/trskop/verbosity/master/dhall/Verbosity/package.dhall

in let
    optionalFold = https://raw.githubusercontent.com/dhall-lang/dhall-haskell/master/Prelude/Optional/fold

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

          , extraHelpMessage = [] : Optional Text

          , verbosity = verbosity.normal
          }

in let
    mkPrefix =
      λ(optionalText : Optional Text)
      → optionalFold Text optionalText Text
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
        }

in  { mkConfig = mkToolsetConfig
    , addSubcommandAliases = addSubcommandAliases
    }
