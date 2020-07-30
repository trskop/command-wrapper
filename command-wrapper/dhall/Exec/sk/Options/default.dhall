-- vim: filetype=dhall

let Options =
      { Type =
            ./Type.dhall sha256:9c93aeda7d25765472107935265ba78477d16d1787b33f0d6db56d27174bb532
          ? ./Type.dhall
      }

let CommonOptions =
      { default =
            ../../CommonOptions/default.dhall sha256:a551a4da0bcc26f8b40b3426cb5121c7fa56e3962686d78e7b787cd2c0c518e6
          ? ../../CommonOptions/default.dhall
      }

let default =
        CommonOptions.default
      ∧ { exactMatch = False
        , regex = False
        , delimiter = None Text
        , reverseInputOrder = False
        , tiebreakSortCriteria = [] : List < Score | Index | Begin | End >
        , interactive = False
        , command = None Text
        , multiSelect = None Bool
        , height = None < Lines : Natural | Percentage : Natural >
        , minHeight = None Natural
        , layout =
            None
              < BottomOfTheScreen
              | TopOfTheScreen
              | TopOfTheScreenPromptAtTheBottom
              >
        , inlineFinderInfo = False
        , prompt = None Text
        , cmdPrompt = None Text
        , header = None Text
        , headerLines = None Natural
        , interpretAnsiCodes = False
        , tabstop = None Natural
        , previewCommand = None Text
        , previewWindowOptions =
          { position = None < Up | Down | Left | Right >
          , size = None < Lines : Natural | Percentage : Natural >
          , wrapLines = False
          , startHidden = False
          }
        , query = None Text
        , cmdQuery = None Text
        , printQuery = False
        , filter = None Text
        , expectKeys = [] : List Text
        , read0 = False
        , print0 = False
        }

let consistency = assert : (Options ∧ { default })::{=} ≡ default

in  default
