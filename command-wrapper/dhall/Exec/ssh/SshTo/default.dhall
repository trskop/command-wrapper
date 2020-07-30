-- vim: filetype=dhall

let SshTo =
      { Type =
            ./Type.dhall sha256:571bf808604f7a1b6854a0120fe3f085451ad9c974b5938004ee9418a254b1ef
          ? ./Type.dhall
      }

let default = { user = None Text, port = None Natural }

let consistency =
        assert
      :   (SshTo ∧ { default })::{ host = "localhost" }
        ≡ default ∧ { host = "localhost" }

in  default
