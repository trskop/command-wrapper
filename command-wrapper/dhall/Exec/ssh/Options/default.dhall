-- vim: filetype=dhall

let Options =
      { Type =
            ./Type.dhall sha256:5c7701e806aeb7526088ef25f1233f2237342a4b1fd617a028f1258983d5202c
          ? ./Type.dhall
      }

let CommonOptions =
      { default =
            ../../CommonOptions/default.dhall sha256:a551a4da0bcc26f8b40b3426cb5121c7fa56e3962686d78e7b787cd2c0c518e6
          ? ../../CommonOptions/default.dhall
      }

let Forwarding =
        ../Forwarding/Type.dhall sha256:434b5e6be35d6dd97cd29665c8c8855be653a25396174de07ec3c158dd8e4cdb
      ? ../Forwarding/Type.dhall

let default =
        CommonOptions.default
      ∧ { forwardings = [] : List Forwarding
        , identityFile = None Text
        , configFile = None Text
        , allocatePseudoTerminal = None Bool
        , doNotExecuteRemoteCommand = False
        , preventReadingOfStdin = False
        }

let consistency = assert : (Options ∧ { default })::{=} ≡ default

in  default
