let CommandWrapper = ./Type/package.dhall

let mkSkel =
        λ ( context
              : { home : Text
                }
          )
      → λ(mkLanguageTemplates : CommandWrapper.SkelMkLangageTemplates)
      → λ(description : Text)
      → λ(wrapper : Text)
      → λ(subcommand : Text)
      → { targetFile = "${context.home}/.local/lib/${wrapper}/${subcommand}"
        , templates = mkLanguageTemplates description wrapper subcommand
        } : CommandWrapper.SkelConfig

in  mkSkel
      : ∀ ( context
              : { home : Text
                }
          )
      → CommandWrapper.SkelMkConfig
