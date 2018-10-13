   let
    Verbosity = ./Verbosity.dhall

in let
    ColourOutput = ./ColourOutput.dhall

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
      , colourOutput : ColourOutput
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
