-- vim: filetype=dhall

let Options =
      { Type =
            ./Type.dhall sha256:82c559b2efa14b05efa1644a6ac53852c45823fd1395f1aa223fd8f9aa549b5c
          ? ./Type.dhall
      , default =
            ./default.dhall sha256:9045715df45ae4d2438d75ea7c934cb3e113716be3e88c52b34a18dfadb8a1a2
          ? ./default.dhall
      }

let CommonOptions =
      { toEnvironment =
            ../../CommonOptions/toEnvironment.dhall sha256:6fa0057412ff539cd27b042cfd1a59fcee06eb64fbff36ca904f0a3436619d35
          ? ../../CommonOptions/toEnvironment.dhall
      }

let Environment =
      { Type =
            ../../../CommandWrapper/Environment/Type sha256:bfc2cb080bb0cac5a42a81beb707437389fa9c6f8e54ae8dd7ce09c6566140ee
          ? ../../../CommandWrapper/Environment/Type
      , empty =
            ../../../CommandWrapper/Environment/empty sha256:cd98ff0deea70057d59baf060c91fba887fc149198b7998ba6581a08c4793a6e
          ? ../../../CommandWrapper/Environment/empty
      }

let toEnvironment =
      λ(_ : Options.Type) →
        CommonOptions.toEnvironment _.{ workingDirectory, environment }

let emptyEnvironmnetByDefault =
      assert : toEnvironment Options.default ≡ Environment.empty

in  toEnvironment : Options.Type → Environment.Type
