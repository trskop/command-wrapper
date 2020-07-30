-- vim: filetype=dhall

{ Type =
      ./Type.dhall sha256:d1c9b4ea8ea803d299311283186f7c1c635dc05c8b8572dbf67dd467009d024d
    ? ./Type.dhall
, default =
      ./default.dhall sha256:fa59f7335ad8b2e11a4f3bdb4987e96379dca2797c3498e4d48db40066f6ecb5
    ? ./default.dhall
, toArguments =
      ./toArguments.dhall sha256:8a2d15f94a126511ce19fd5818781da92ed4adcb6e06d04dd8b3430290b97051
    ? ./toArguments.dhall
, toEnvironment =
      ./toEnvironment.dhall sha256:a89ccf47eec4ababadd0f49dd2cddf6eab3fbb6bba52bfb0aa1f2f4b618d434b
    ? ./toEnvironment.dhall
}
