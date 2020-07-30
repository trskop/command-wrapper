-- vim: filetype=dhall

let Options =
      { Type =
            ./Type.dhall sha256:674f9d48d01ccb653cd071dd30af0766337d30d0c8fa11498ab3a6e91219479a
          ? ./Type.dhall
      }

let CommonOptions =
      { default =
            ../../CommonOptions/default.dhall sha256:a551a4da0bcc26f8b40b3426cb5121c7fa56e3962686d78e7b787cd2c0c518e6
          ? ../../CommonOptions/default.dhall
      }

let default = CommonOptions.default ∧ { noPager = False, noRun = False }

let consistency = assert : (Options ∧ { default })::{=} ≡ default

in  default
