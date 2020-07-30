-- vim: filetype=dhall

let Options =
      { Type =
            ./Type.dhall sha256:e908cbddb6ec0b95462e513b70eaf6181aed7908212ad1f5614ee736bc174c9c
          ? ./Type.dhall
      , default =
            ./default.dhall sha256:ce42936b737e0d7ccd23c499b1484179bd083c5a4f31749ab46aed2402cd1adc
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
