-- vim: filetype=dhall

let Options =
      { Type =
            ./Type.dhall sha256:4a4dda0eaf771745c5bfdefb80cdf489c87385626c9501ee19e00485bca4b48f
          ? ./Type.dhall
      }

let CommonOptions =
      { default =
            ../../CommonOptions/default.dhall sha256:a551a4da0bcc26f8b40b3426cb5121c7fa56e3962686d78e7b787cd2c0c518e6
          ? ../../CommonOptions/default.dhall
      }

let default = CommonOptions.default ∧ { verbosity = None < silent | verbose > }

let consistency = assert : (Options ∧ { default })::{=} ≡ default

in  default
