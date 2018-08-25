  let
    Command = ./Command.dhall

in let
    CdConfig =
      { directories : List Text
      , menuTool : Text
      , shell : Text
      , terminalEmulator : ∀(directory : Text) → Command
      }

in let
    CdMkConfig =
        ∀ ( update
              : ∀(default : CdConfig) → CdConfig
          )
      → CdConfig

in let
    Cd =
      { Config = CdConfig
      , MkConfig = CdMkConfig
      }

in  Cd
      : { Config : Type
        , MkConfig : Type
        }
