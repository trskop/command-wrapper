let SkelLanguage : Type = <Bash | Dhall | Haskell >

let SkelTemplate : Type =
      { targetFile : Text
      , executable : Bool
      , template : Text
      }

let SkelConfig : Type =
      { template : ∀(language : SkelLanguage) → SkelTemplate

      -- Open the newly created file in an editor?
      , editAfterwards : Bool
      }

let SkelMkConfig : Type =
        ∀(wrapper : Text)     -- Name of the TOOLSET_COMMAND
      → ∀(subcommand : Text)  -- Name of the SUBCOMMAND
      → ∀(command : Text)     -- "${TOOLSET_COMMAND}-${SUBCOMMAND}"
      → SkelConfig

in  { Language = SkelLanguage
    , Template = SkelTemplate
    , Config = SkelConfig
    , MkConfig = SkelMkConfig
    }
