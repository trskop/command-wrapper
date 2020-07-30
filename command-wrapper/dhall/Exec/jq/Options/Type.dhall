-- vim: filetype=dhall

let CommonOptions =
        ../../CommonOptions/Type.dhall sha256:2df1fcace0ed3c72e1bcf3c0e6b9ae0269c36315ced7961c62f7ba59d426d6eb
      ? ../../CommonOptions/Type.dhall

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

let JqOptions =
      { inputType : Optional InputType
      , outputType : Optional OutputType
      , asciiOutput : Bool
      , compactOutput : Bool
      , sortKeys : Bool
      , variables : List Variable
      , outputColour : Optional OutputColour
      }

in  CommonOptions ⩓ JqOptions
