-- vim: filetype=dhall

let Options =
      { Type =
            ./Type.dhall sha256:89bafb720da9507168156fc176d0216d081875e79b1a23f8c301fdeb3721bb6d
          ? ./Type.dhall
      , default =
            ./default.dhall sha256:a551a4da0bcc26f8b40b3426cb5121c7fa56e3962686d78e7b787cd2c0c518e6
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
        # [ { name = "JIRA_API_TOKEN", value = _.token } ]

let test0 =
        assert
      :   toEnvironment
            Options::{
            , baseUrl = "https://example.com/v1"
            , login = "user@example.com"
            , user = "user"
            , token = "token"
            }
        ≡ [ { name = "JIRA_API_TOKEN", value = "token" } ]

let test1 =
        assert
      :   toEnvironment
            Options::{
            , baseUrl = "https://example.com/v1"
            , login = "user@example.com"
            , user = "user"
            , token = "token"
            , environment = [ { name = "FOO", value = "bar" } ]
            }
        ≡ [ { name = "FOO", value = "bar" }
          , { name = "JIRA_API_TOKEN", value = "token" }
          ]

in  toEnvironment : Options.Type → Environment.Type
