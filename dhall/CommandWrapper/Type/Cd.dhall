let CommandWithEnvironment = ./CommandWithEnvironment.dhall

let CdConfig =
      { directories : List Text
      , menuTool : CommandWithEnvironment
      , shell : Text
      , terminalEmulator : ∀(directory : Text) → CommandWithEnvironment
      }

let CdMkConfig =
        ∀ ( update
              : ∀(default : CdConfig) → CdConfig
          )
      → CdConfig

let Cd =
      { Config = CdConfig
      , MkConfig = CdMkConfig
      }

in  Cd
      : { Config : Type
        , MkConfig : Type
        }
