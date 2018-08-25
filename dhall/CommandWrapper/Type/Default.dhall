   let
    Verbosity = https://raw.githubusercontent.com/trskop/verbosity/master/dhall/Verbosity.dhall

in let
    DefaultConfig =
      { aliases
          : List
              { alias : Text
              , command : Text
              , arguments : List Text
              }
      , searchPath : List Text
      , extraHelpMessage : Optional Text
      , verbosity : Verbosity
      }

in let
    DefaultMkConfig =
        ∀ ( update
              :  ∀(default : DefaultConfig) → DefaultConfig
          )
      → DefaultConfig

in let
    Default =
      { Config = DefaultConfig
      , MkConfig = DefaultMkConfig
      }

in  Default
      : { Config : Type
        , MkConfig : Type
        }
