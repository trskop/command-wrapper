-- vim: filetype=dhall

let Options =
      { Type =
            ./Type.dhall sha256:7cbb3d15d2d57539fe0274c8c1223148bb7b34d0310e0c58d4ab7f49e7e0ca5f
          ? ./Type.dhall
      , default =
            ./default.dhall sha256:fcc7c0bfd22e1c1cc54cfe6249340f56410e296a90da6720156af78b7b7365ca
          ? ./default.dhall
      }

let Environment =
      { Type =
            ../../../CommandWrapper/Environment/Type sha256:bfc2cb080bb0cac5a42a81beb707437389fa9c6f8e54ae8dd7ce09c6566140ee
          ? ../../../CommandWrapper/Environment/Type
      , empty =
            ../../../CommandWrapper/Environment/empty sha256:cd98ff0deea70057d59baf060c91fba887fc149198b7998ba6581a08c4793a6e
          ? ../../../CommandWrapper/Environment/empty
      }

let optionalEnvironmentVariables =
        ../../utils/optionalEnvironmentVariables.dhall sha256:2565c61b55b84de6e75b663f3561c806bcebc193ff662c644150dc46ebe05c0b
      ? ../../utils/optionalEnvironmentVariables.dhall

let toEnvironment =
      λ(options : Options.Type) →
          optionalEnvironmentVariables
            Text
            ( λ(_ : Text) →
                [ { name = "COMMAND_WRAPPER_INVOKE_AS", value = _ } ]
            )
            options.invokeAs
        # optionalEnvironmentVariables
            Text
            (λ(_ : Text) → [ { name = "COMMAND_WRAPPER_PATH", value = _ } ])
            options.path
        # optionalEnvironmentVariables
            Text
            (λ(_ : Text) → [ { name = "COMMAND_WRAPPER_MANPATH", value = _ } ])
            options.manPath
        # optionalEnvironmentVariables
            Text
            (λ(_ : Text) → [ { name = "XDG_CONFIG_HOME", value = _ } ])
            options.configHome

let noArgumentsByDefault =
      assert : toEnvironment Options.default ≡ Environment.empty

in  toEnvironment : Options.Type → Environment.Type
