-- vim: filetype=dhall

let Options =
      { Type =
            ./Type.dhall sha256:54460863b32e729cf617282abfc10164e5ac65565bb8b71099b27567e6404a3d
          ? ./Type.dhall
      }

let CommonOptions =
      { default =
            ../../CommonOptions/default.dhall sha256:a551a4da0bcc26f8b40b3426cb5121c7fa56e3962686d78e7b787cd2c0c518e6
          ? ../../CommonOptions/default.dhall
      }

let default =
        CommonOptions.default
      ∧ { extended = None Bool
        , exactMatch = None Bool
        , algorithm = None < v1 | v2 >
        , caseSensitive =
            < SmartCase | CaseInsensitive | CaseSensitive >.SmartCase
        , delimiter = None Text
        , sortResult = None Bool
        , reverseInputOrder = None Bool
        , tiebreakSortCriteria = [] : List < Length | Begin | End | Index >
        , multiSelect = None Bool
        , noMouse = False
        , cycleScroll = None Bool
        , respectFilepathSeparator = None Bool
        , height = None < Lines : Natural | Percentage : Natural >
        , minHeight = None Natural
        , layout =
            None
              < BottomOfTheScreen
              | TopOfTheScreen
              | TopOfTheScreenPromptAtTheBottom
              >
        , drawBorder = None Bool
        , useUnicodeCharacters = None Bool
        , inlineFinderInfo = None Bool
        , prompt = None Text
        , header = None Text
        , headerLines = None Natural
        , interpretAnsiCodes = None Bool
        , useBoldText = None Bool
        , blackBackground = None Bool
        , historyFile = None Text
        , historySize = None Natural
        , previewCommand = None Text
        , previewWindowOptions =
          { position = None < Up | Down | Left | Right >
          , size = None < Lines : Natural | Percentage : Natural >
          , wrapLines = False
          , startHidden = False
          }
        , query = None Text
        , select1 = None Bool
        , exit0 = None Bool
        , filter = None Text
        , printQuery = None Bool
        , expectKeys = [] : List Text
        , read0 = None Bool
        , print0 = None Bool
        , synchronous = None Bool
        }

let consistency = assert : (Options ∧ { default })::{=} ≡ default

in  default
