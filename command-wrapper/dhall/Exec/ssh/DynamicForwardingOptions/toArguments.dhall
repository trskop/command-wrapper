-- vim: filetype=dhall

let DynamicForwardingOptions =
      { Type =
            ./Type.dhall sha256:d4ebcc6427498a1408b5772b63cbace514cac027d0c6d9cddda6095529aa8a56
          ? ./Type.dhall
      , default =
            ./default.dhall sha256:c80d7d2fddac6d33d118c485dde6ec80332bf59174b83cfb5b85318ad631ea01
          ? ./default.dhall
      }

let toArguments
    : Text → DynamicForwardingOptions.Type → List Text
    = λ(option : Text) →
      λ(dynamicForwarding : DynamicForwardingOptions.Type) →
        [ option
        ,     merge
                { None = "", Some = λ(_ : Text) → "${_}:" }
                dynamicForwarding.bindTo
          ++  Natural/show dynamicForwarding.listenOn
        ]

let example0 =
        assert
      :   toArguments "-D" DynamicForwardingOptions::{ listenOn = 8080 }
        ≡ [ "-D", "8080" ]

let example1 =
        assert
      :   toArguments
            "-D"
            DynamicForwardingOptions::{
            , bindTo = Some "localhost"
            , listenOn = 8080
            }
        ≡ [ "-D", "localhost:8080" ]

in  toArguments
