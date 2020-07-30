-- vim: filetype=dhall

let Options =
      { Type =
            ./Type.dhall sha256:e908cbddb6ec0b95462e513b70eaf6181aed7908212ad1f5614ee736bc174c9c
          ? ./Type.dhall
      }

let CommonOptions =
      { default =
            ../../CommonOptions/default.dhall sha256:a551a4da0bcc26f8b40b3426cb5121c7fa56e3962686d78e7b787cd2c0c518e6
          ? ../../CommonOptions/default.dhall
      }

let default =
        CommonOptions.default
      ∧ { lines = 0
        , prompt = None Text
        , showScores = False
        , tty = None Text
        , query = None Text
        , showMatches = None Text
        , workers = 0
        }

let consistency = assert : (Options ∧ { default })::{=} ≡ default

in  default
