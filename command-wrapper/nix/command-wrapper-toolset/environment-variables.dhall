''
let Variables =
        { `${TOOLSET}_BASH_COMPLETION` : Text
        , `${TOOLSET}_FISH_COMPLETION` : Text
        , `${TOOLSET}_ZSH_COMPLETION` : Text
        }
      ⩓ Global.Type

let default =
        { `${TOOLSET}_BASH_COMPLETION` =
            "${out}/share/bash-completion/completions/${toolset}.bash"
        , `${TOOLSET}_FISH_COMPLETION` =
            "${out}/share/fish/vendor_completions.d/${toolset}.fish"
        , `${TOOLSET}_ZSH_COMPLETION` =
            "${out}/share/zsh/vendor_completions/_${toolset}"
        }
      ∧ Global.default

let toList
    : Variables → List CommandWrapper.EnvironmentVariable.Type
    = λ(variables : Variables) →
        Prelude.List.map
          { mapKey : Text, mapValue : Text }
          CommandWrapper.EnvironmentVariable.Type
          ( λ(x : { mapKey : Text, mapValue : Text }) →
              { name = x.mapKey, value = x.mapValue }
          )
          (toMap variables)

let consistency0 = assert : { Type = Variables, default }::{=} ≡ default

in  { Type = Variables, default, toList }
''
