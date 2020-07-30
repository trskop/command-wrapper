-- vim: filetype=dhall

let Options =
      { Type =
            ./Type.dhall sha256:2df1fcace0ed3c72e1bcf3c0e6b9ae0269c36315ced7961c62f7ba59d426d6eb
          ? ./Type.dhall
      }

let CommonOptions =
      { default =
            ../../CommonOptions/default.dhall sha256:a551a4da0bcc26f8b40b3426cb5121c7fa56e3962686d78e7b787cd2c0c518e6
          ? ../../CommonOptions/default.dhall
      }

let default = CommonOptions.default ∧ {=}

let consistency = assert : (Options ∧ { default })::{=} ≡ default

in  default
