-- vim: filetype=dhall

{ Type =
      ./Type.dhall sha256:571bf808604f7a1b6854a0120fe3f085451ad9c974b5938004ee9418a254b1ef
    ? ./Type.dhall
, default =
      ./default.dhall sha256:75276d84f6a69b74acb77dd68dec486f6e675dd8646fedbfa236a4292e9950a8
    ? ./default.dhall
}
