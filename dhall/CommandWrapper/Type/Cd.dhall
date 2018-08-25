  let
    CdConfig =
      { directories : List Text
      , menuTool : Text
      , shell : Text
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
