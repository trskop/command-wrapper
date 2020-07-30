-- vim: filetype=dhall

{ Type =
      ./Type.dhall sha256:d4ebcc6427498a1408b5772b63cbace514cac027d0c6d9cddda6095529aa8a56
    ? ./Type.dhall
, default =
      ./default.dhall sha256:c80d7d2fddac6d33d118c485dde6ec80332bf59174b83cfb5b85318ad631ea01
    ? ./default.dhall
, toArguments =
      ./toArguments.dhall sha256:ccb7a2991cd8dcf56f69ed5960573559cbc2c1a237b73ab5d9864ef5ab0a5277
    ? ./toArguments.dhall
}
