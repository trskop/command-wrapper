-- vim: filetype=dhall

{ Type =
      ./Type.dhall sha256:c43df7ef522e86341c59087914011ef5deadeba6b9363665a63ba5e15aa3647f
    ? ./Type.dhall
, default =
      ./default.dhall sha256:44ddaa795338e389c4ec7ffdc4b43e6581e4718913f40408c0ada14a6a2179d0
    ? ./default.dhall
, toArguments =
      ./toArguments.dhall sha256:29a409b9ddf96c57e7aad77cd1a5f8cfd0d3d5ce35b1e01849923f8aff75d10b
    ? ./toArguments.dhall
, interactive =
      ./interactive.dhall sha256:66b78ada2f3cf1107f9dcf719bc79f9a0adba9d94bb57f8c512d6adfe2d5b78c
    ? ./interactive.dhall
, ephemeral =
      ./ephemeral.dhall sha256:38787dc026b92a013937026c9096b1fa65cbf6becdbd7e55764aa71a23bac345
    ? ./ephemeral.dhall
}
