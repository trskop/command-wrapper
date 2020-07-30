-- vim: filetype=dhall

let Options =
      { Type =
            ./Type.dhall sha256:dbef3a3dd77c5377d0f571fde20161110ea5f99383ee7ac3c97cdee6b72cab67
          ? ./Type.dhall
      }

let LogLevel =
        ../LogLevel/Type.dhall sha256:f63d8d8d8c27e7edd9b623e5abea25b28f94293d2461347c718782ae09dfc41b
      ? ../LogLevel/Type.dhall

let CommonOptions =
      { default =
            ../../CommonOptions/default.dhall sha256:a551a4da0bcc26f8b40b3426cb5121c7fa56e3962686d78e7b787cd2c0c518e6
          ? ../../CommonOptions/default.dhall
      }

let default =
        CommonOptions.default
      ∧ { host = [] : List Text
        , logLevel = None LogLevel
        , config = None Text
        , context = None Text
        , debug = False
        }

let consistency = assert : (Options ∧ { default })::{=} ≡ default

in  default
