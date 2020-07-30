-- vim: filetype=dhall

let Options =
      { Type =
            ./Type.dhall sha256:d1c9b4ea8ea803d299311283186f7c1c635dc05c8b8572dbf67dd467009d024d
          ? ./Type.dhall
      }

let CommonOptions =
      { default =
            ../../CommonOptions/default.dhall sha256:a551a4da0bcc26f8b40b3426cb5121c7fa56e3962686d78e7b787cd2c0c518e6
          ? ../../CommonOptions/default.dhall
      }

let emptyEnvironment =
        ../../../CommandWrapper/Environment/empty sha256:cd98ff0deea70057d59baf060c91fba887fc149198b7998ba6581a08c4793a6e
      ? ../../../CommandWrapper/Environment/empty

let default =
        CommonOptions.default
      ∧ { ignoreEnvironment = False
        , define = emptyEnvironment
        , undefine = [] : List Text
        }

let consistency = assert : (Options ∧ { default })::{=} ≡ default

in  default
