let Verbosity = ./Verbosity.dhall

let ColourOutput = ./ColourOutput.dhall

let DefaultConfig =
      { aliases
          : List
              { alias : Text
              , command : Text
              , arguments : List Text
              }
      , searchPath : List Text
      , description : Optional Text
      , extraHelpMessage : Optional Text
      , verbosity : Verbosity
      , colourOutput : Optional ColourOutput
      }

let DefaultMkConfig =
        ∀ ( update
              :  ∀(default : DefaultConfig) → DefaultConfig
          )
      → DefaultConfig

let Default =
      { Config = DefaultConfig
      , MkConfig = DefaultMkConfig
      }

in  Default
      : { Config : Type
        , MkConfig : Type
        }
