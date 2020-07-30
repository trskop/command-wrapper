-- vim: filetype=dhall

{ Type =
      ./Type.dhall sha256:e8382f5875b4a311bd30919586963a291cd2827be369dbad7cc6247212f2f96d
    ? ./Type.dhall
, default =
      ./default.dhall sha256:9bb9dcb5bf6f795291686f59383bcd01c8e79b87fc3fb63351d46dea100ac51b
    ? ./default.dhall
}
