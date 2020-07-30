-- vim: filetype=dhall

{ Type =
      ./Type.dhall sha256:f060f3aa27210e24ea2ff0fd222b3c0d4c376f79077d376c3b40596ad7d3d74a
    ? ./Type.dhall
, default =
      ./default.dhall sha256:ecfa3148efb09aa997b99ed97df7f7f1e0184e251b56d9c5407021d35d008fdd
    ? ./default.dhall
, toArguments =
      ./toArguments.dhall sha256:6537d30b5f6d9aa6bcd5526659273deb224f4296cdc565b0b3d948b92d67e1b2
    ? ./toArguments.dhall
, interactive =
      ./interactive.dhall sha256:fb70fe482f04a1bd4a2b3e639328356b1212dc118a1015e646ad0de93fd95823
    ? ./interactive.dhall
}
