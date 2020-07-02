-- vim: filetype=dhall

{ Options =
      ./Options/package.dhall sha256:8fb57c0027ea1085bdc4218cb822c1b4ee5ed46ded4a68b25bf0f92d76857252
    ? ./Options/package.dhall
, Profile =
      ./Profile/package.dhall sha256:35dad17584418559d8fb0aeef20b0d30fa9f1531e796529aecdd4d0c64343cf4
    ? ./Profile/package.dhall
, Remote =
      ./Remote/package.dhall sha256:8c6587890d581890623d362d12a41c210ac57a4eef87e196214e6bae73dd41e6
    ? ./Remote/package.dhall
, Open =
      ./Open/package.dhall sha256:63eaffa659febaf6950dcfa7a40147772d1cf5c4286f40937e8c7bc904ad968f
    ? ./Open/package.dhall
, command =
      ./command sha256:9e66863cd004fe6d5037c4c15bd71865cb676c5d1e922b6fa1d6e7b827b6859f
    ? ./command
}
