let SkelTemplate =
      { targetFile : Optional Text
      , executable : Bool
      , template : Text
      }

let SkelLanguageTemplates =
      { haskell : SkelTemplate
      , bash : SkelTemplate
      }

let SkelConfig =
      { targetFile : Text
      , templates : SkelLanguageTemplates
      }

let SkelMkLangageTemplates =
        ∀(description : Text)
      → ∀(wrapper : Text)
      → ∀(subcommand : Text)
      → SkelLanguageTemplates

let SkelMkConfig =
        ∀(mkLanguageTemplates : SkelMkLangageTemplates)
      → ∀(description : Text)
      → ∀(wrapper : Text)
      → ∀(subcommand : Text)
      → SkelConfig

let Skel =
      { Template = SkelTemplate
      , LanguageTemplates = SkelLanguageTemplates
      , Config = SkelConfig
      , MkLangageTemplates = SkelMkLangageTemplates
      , MkConfig = SkelMkConfig
      }

in  Skel
      : { Template : Type
        , LanguageTemplates : Type
        , Config : Type
        , MkLangageTemplates : Type
        , MkConfig : Type
        }
