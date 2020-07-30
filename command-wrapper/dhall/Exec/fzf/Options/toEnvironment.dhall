-- vim: filetype=dhall

let Options =
      { Type =
            ./Type.dhall sha256:54460863b32e729cf617282abfc10164e5ac65565bb8b71099b27567e6404a3d
          ? ./Type.dhall
      , default =
            ./default.dhall sha256:e7779cc66f4072ec1586cc99158ee19641c282c3c03d6d42363876695c5af8ec
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
