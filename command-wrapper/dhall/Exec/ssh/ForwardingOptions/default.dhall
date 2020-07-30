-- vim: filetype=dhall

let ForwardingOptions =
      { Type =
            ./Type.dhall sha256:ef6300632529b203c5a24b33d92ed0c5c4d05cd0c72655ce768271a2e6950f9f
          ? ./Type.dhall
      }

let ListenOn =
        ../ListenOn/Type.dhall sha256:8f9d7c5e14dcc61cb0437a17ec268328d44a9b1aa1c130c1a9b48e55644c8d0e
      ? ../ListenOn/Type.dhall

let ConnectTo =
      { Type =
            ../ConnectTo/Type.dhall sha256:e8382f5875b4a311bd30919586963a291cd2827be369dbad7cc6247212f2f96d
          ? ../ConnectTo/Type.dhall
      , default =
            ../ConnectTo/default.dhall sha256:9bb9dcb5bf6f795291686f59383bcd01c8e79b87fc3fb63351d46dea100ac51b
          ? ../ConnectTo/default.dhall
      }

let default = {=}

let consistency =
        assert
      :   (ForwardingOptions ∧ { default })::{
          , listenOn = ListenOn.UnixSocket { path = "/path/to/socket" }
          , connectTo = ConnectTo::{ host = "localhost", port = 443 }
          }
        ≡   default
          ∧ { listenOn = ListenOn.UnixSocket { path = "/path/to/socket" }
            , connectTo = ConnectTo::{ host = "localhost", port = 443 }
            }

in  default
