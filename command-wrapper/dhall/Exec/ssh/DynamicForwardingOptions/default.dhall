-- vim: filetype=dhall

let DynamicForwardingOptions =
      { Type =
            ./Type.dhall sha256:d4ebcc6427498a1408b5772b63cbace514cac027d0c6d9cddda6095529aa8a56
          ? ./Type.dhall
      }

let default = { bindTo = None Text }

let consistency =
        assert
      :   (DynamicForwardingOptions ∧ { default })::{ listenOn = 8080 }
        ≡ default ∧ { listenOn = 8080 }

in  default
