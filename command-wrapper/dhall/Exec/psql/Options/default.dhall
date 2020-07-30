-- vim: filetype=dhall

let Options =
      { Type =
            ./Type.dhall sha256:61553461550695b5b8c52861fff9ceecf0c70ebd703cfa2f7465d97aca6d3a66
          ? ./Type.dhall
      }

let CommonOptions =
      { default =
            ../../CommonOptions/default.dhall sha256:a551a4da0bcc26f8b40b3426cb5121c7fa56e3962686d78e7b787cd2c0c518e6
          ? ../../CommonOptions/default.dhall
      }

let ColourOutput =
        ../../../CommandWrapper/ColourOutput/Type sha256:0ef91a3f044406ee80fc20f26127b448a0e98f46c46ec024457023d2aded8543
      ? ../../../CommandWrapper/ColourOutput/Type

let default =
        CommonOptions.default
      ∧ { pgpassFile = None Text
        , psqlrcFile = None Text
        , quiet = False
        , verbosity = None < default | verbose | terse >
        , colour = None ColourOutput
        , showContext = None < always | errors | never >
        , editor = None Text
        , pager = None Text
        }

let consistency = assert : (Options ⫽ { default })::{=} ≡ default

in  default
