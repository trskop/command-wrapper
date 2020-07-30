-- vim: filetype=dhall

let Options =
      { Type =
            ./Type.dhall sha256:b583193040d153654a70a31291aa959845a9d3246c367e65567b3d96dd3c6730
          ? ./Type.dhall
      }

let CommonOptions =
      { default =
            ../../CommonOptions/default.dhall sha256:a551a4da0bcc26f8b40b3426cb5121c7fa56e3962686d78e7b787cd2c0c518e6
          ? ../../CommonOptions/default.dhall
      }

let default = CommonOptions.default ∧ { term = None Text, config = None Text }

let consistency = assert : (Options ∧ { default })::{=} ≡ default

in  default
