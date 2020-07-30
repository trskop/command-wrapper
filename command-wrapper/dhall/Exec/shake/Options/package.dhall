-- vim: filetype=dhall

{ Type =
      ./Type.dhall sha256:7a7eb5cc89d0f8434982f3c8c6e95026eab92e5b13e8223b8e2ef0066ef5cd3d
    ? ./Type.dhall
, default =
      ./default.dhall sha256:179b7a2158c765d02808b502bdb6fc40c20879852e7965c8b0b8345ba9afe269
    ? ./default.dhall
, toArguments =
      ./toArguments.dhall sha256:f332cf28a3372927273a36cf7468e49ae52eed2962083b16d0a5354a520c48bd
    ? ./toArguments.dhall
}
