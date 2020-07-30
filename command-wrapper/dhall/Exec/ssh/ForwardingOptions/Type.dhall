-- vim: filetype=dhall

let ListenOn =
        ../ListenOn/Type.dhall sha256:8f9d7c5e14dcc61cb0437a17ec268328d44a9b1aa1c130c1a9b48e55644c8d0e
      ? ../ListenOn/Type.dhall

let ConnectTo =
        ../ConnectTo/Type.dhall sha256:e8382f5875b4a311bd30919586963a291cd2827be369dbad7cc6247212f2f96d
      ? ../ConnectTo/Type.dhall

in  { listenOn : ListenOn, connectTo : ConnectTo }
