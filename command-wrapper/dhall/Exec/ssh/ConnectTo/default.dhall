-- vim: filetype=dhall

let ConnectTo =
      { Type =
            ./Type.dhall sha256:e8382f5875b4a311bd30919586963a291cd2827be369dbad7cc6247212f2f96d
          ? ./Type.dhall
      }

let default = {=}

let consistency =
        assert
      :   (ConnectTo ∧ { default })::{ host = "localhost", port = 22 }
        ≡ default ∧ { host = "localhost", port = 22 }

in  default
