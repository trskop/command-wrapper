-- vim: filetype=dhall

let Options =
      { Type =
            ./Type.dhall sha256:89bafb720da9507168156fc176d0216d081875e79b1a23f8c301fdeb3721bb6d
          ? ./Type.dhall
      }

let CommonOptions =
      { default =
            ../../CommonOptions/default.dhall sha256:a551a4da0bcc26f8b40b3426cb5121c7fa56e3962686d78e7b787cd2c0c518e6
          ? ../../CommonOptions/default.dhall
      }

let default = CommonOptions.default

let consistency =
        assert
      :   (Options ∧ { default })::{
          , baseUrl = "https://example.com"
          , login = "user@example.com"
          , user = "user"
          , token = "token"
          }
        ≡   default
          ∧ { baseUrl = "https://example.com"
            , login = "user@example.com"
            , user = "user"
            , token = "token"
            }

in  default
