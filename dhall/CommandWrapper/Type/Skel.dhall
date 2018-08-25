  let
    SkelTemplate =
      { targetFile : Optional Text
      , executable : Bool
      , template : Text
      }

in let
    SkelLanguageTemplates =
      { haskell : SkelTemplate
      , bash : SkelTemplate
      }

in let
    SkelConfig =
      { targetFile : Text
      , templates : SkelLanguageTemplates
      }

in let
    SkelMkLangageTemplates =
        ∀(description : Text)
      → ∀(wrapper : Text)
      → ∀(subcommand : Text)
      → SkelLanguageTemplates

in let
    SkelMkConfig =
        ∀(mkLanguageTemplates : SkelMkLangageTemplates)
      → ∀(description : Text)
      → ∀(wrapper : Text)
      → ∀(subcommand : Text)
      → SkelConfig

in let
    Skel =
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
