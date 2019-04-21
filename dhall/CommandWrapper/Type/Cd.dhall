let Command : Type = ./Command.dhall

let CommandWithEnvironment : Type = ./CommandWithEnvironment.dhall

let CdConfig : Type =
      { directories : List Text

      , menuTool
          : ∀(query : Optional Text)
          → CommandWithEnvironment

      -- When specified as `None Text` system shell is used.
      , shell : Optional Text

      , terminalEmulator
          : ∀(directory : Text)
          → ∀(command : Optional Command)
          → CommandWithEnvironment
      }

let CdMkConfig : Type =
      ∀(update : CdConfig → CdConfig) → CdConfig

let Cd =
      { Config = CdConfig
      , MkConfig = CdMkConfig
      }

in  Cd
      : { Config : Type
        , MkConfig : Type
        }
