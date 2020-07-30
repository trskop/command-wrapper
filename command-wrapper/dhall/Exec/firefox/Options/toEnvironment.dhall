-- vim: filetype=dhall

let Options =
      { Type =
            ./Type.dhall sha256:054af9262f73048f90af26b55ff29e9d6cb0b4b7f8002bdb1994495636f4cc84
          ? ./Type.dhall
      , default =
            ./default.dhall sha256:430f666c22a7337f0f016178b33a11479041b5bf168ed9779703b074d630eb99
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
