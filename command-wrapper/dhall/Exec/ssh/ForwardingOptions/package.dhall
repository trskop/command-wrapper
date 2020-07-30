-- vim: filetype=dhall

{ Type =
      ./Type.dhall sha256:ef6300632529b203c5a24b33d92ed0c5c4d05cd0c72655ce768271a2e6950f9f
    ? ./Type.dhall
, default =
      ./default.dhall sha256:9bb9dcb5bf6f795291686f59383bcd01c8e79b87fc3fb63351d46dea100ac51b
    ? ./default.dhall
, toArguments =
      ./toArguments.dhall sha256:70919deed071696eb9f4a592f91a5329d211d1fcc35e1358fde5e8824ba31c9a
    ? ./toArguments.dhall
}
