-- vim: filetype=dhall

let Forwarding =
        ./Type.dhall sha256:434b5e6be35d6dd97cd29665c8c8855be653a25396174de07ec3c158dd8e4cdb
      ? ./Type.dhall

let ForwardingOptions =
      { toArguments =
            ../ForwardingOptions/toArguments.dhall sha256:70919deed071696eb9f4a592f91a5329d211d1fcc35e1358fde5e8824ba31c9a
          ? ../ForwardingOptions/toArguments.dhall
      }

let DynamicForwardingOptions =
      { Type =
            ../DynamicForwardingOptions/Type.dhall sha256:d4ebcc6427498a1408b5772b63cbace514cac027d0c6d9cddda6095529aa8a56
          ? ../DynamicForwardingOptions/Type.dhall
      , default =
            ../DynamicForwardingOptions/default.dhall sha256:c80d7d2fddac6d33d118c485dde6ec80332bf59174b83cfb5b85318ad631ea01
          ? ../DynamicForwardingOptions/default.dhall
      , toArguments =
            ../DynamicForwardingOptions/toArguments.dhall sha256:ccb7a2991cd8dcf56f69ed5960573559cbc2c1a237b73ab5d9864ef5ab0a5277
          ? ../DynamicForwardingOptions/toArguments.dhall
      }

let toArguments =
      λ(forwarding : Forwarding) →
        merge
          { Local = ForwardingOptions.toArguments "-L"
          , Remote = ForwardingOptions.toArguments "-R"
          , Dynamic = DynamicForwardingOptions.toArguments "-D"
          }
          forwarding

let example0 =
        assert
      :   toArguments
            (Forwarding.Dynamic DynamicForwardingOptions::{ listenOn = 8080 })
        ≡ [ "-D", "8080" ]

in  toArguments : Forwarding → List Text
