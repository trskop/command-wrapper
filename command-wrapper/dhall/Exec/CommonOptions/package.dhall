-- vim: filetype=dhall

{ Type =
      ./Type.dhall sha256:2df1fcace0ed3c72e1bcf3c0e6b9ae0269c36315ced7961c62f7ba59d426d6eb
    ? ./Type.dhall
, default =
      ./default.dhall sha256:a551a4da0bcc26f8b40b3426cb5121c7fa56e3962686d78e7b787cd2c0c518e6
    ? ./default.dhall
, toEnvironment =
      ./toEnvironment.dhall sha256:6fa0057412ff539cd27b042cfd1a59fcee06eb64fbff36ca904f0a3436619d35
    ? ./toEnvironment.dhall
}
