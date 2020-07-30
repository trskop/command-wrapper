-- vim: filetype=dhall

let Options =
      { Type =
            ./Type.dhall sha256:d10faab8c8388e68d0e931248dee4741cbcef8b593095d8dd8a42a54c2272ab5
          ? ./Type.dhall
      }

let CommonOptions =
      { default =
            ../../CommonOptions/default.dhall sha256:a551a4da0bcc26f8b40b3426cb5121c7fa56e3962686d78e7b787cd2c0c518e6
          ? ../../CommonOptions/default.dhall
      }

let Action =
        ../Action/Type.dhall sha256:8df4bf8c0bcc06dae0cc7235ab731ddf210b072ea8ece9ff96b8abc5d138f36b
      ? ../Action/Type.dhall

let default =
        CommonOptions.default
      ∧ { silent = False
        , nostart = False
        , beforeCommand = None Text
        , afterCommand = None Text
        , serverName = None Text
        , diffMode = False
        , previousWindow = False
        , action = None Action
        }

let consistency = assert : (Options ∧ { default })::{=} ≡ default

in  default
