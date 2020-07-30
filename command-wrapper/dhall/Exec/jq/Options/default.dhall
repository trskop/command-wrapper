-- vim: filetype=dhall

let Options =
      { Type =
            ./Type.dhall sha256:888713faabcc80155c73e123229956496a179aeb6737d9c460107ab0b59c373f
          ? ./Type.dhall
      }

let CommonOptions =
      { default =
            ../../CommonOptions/default.dhall sha256:a551a4da0bcc26f8b40b3426cb5121c7fa56e3962686d78e7b787cd2c0c518e6
          ? ../../CommonOptions/default.dhall
      }

let InputType =
        ../InputType/Type.dhall sha256:6d1a56a4bc24cfca6a6470a1e2c781b2092c60b64aff6cc19a341827e447e1a9
      ? ../InputType/Type.dhall

let OutputType =
        ../OutputType/Type.dhall sha256:070309fd254a430683ba3700b29075cdf60c688fc5a82e0a4ab62c2605cd815d
      ? ../OutputType/Type.dhall

let Variable =
        ../Variable/Type.dhall sha256:81af69a61b5774cc7c4c4902d08c60b0e88ddd62d28af37e346afb79a7e1aab1
      ? ../Variable/Type.dhall

let OutputColour =
        ../OutputColour/Type.dhall sha256:a7a3fce54d436216a0f504bfda5f3b67cb07c171209e9d904a9aa484507297fa
      ? ../OutputColour/Type.dhall

let default =
        CommonOptions.default
      ∧ { inputType = None InputType
        , outputType = None OutputType
        , asciiOutput = False
        , compactOutput = False
        , sortKeys = False
        , variables = [] : List Variable
        , outputColour = None OutputColour
        }

let consistency = assert : (Options ∧ { default })::{=} ≡ default

in  default
