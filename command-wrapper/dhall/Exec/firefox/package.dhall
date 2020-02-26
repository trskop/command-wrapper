-- vim: filetype=dhall

{ Options =
      ./Options/package.dhall sha256:bc4282f67b1eb6845e0b534d067f79c9532cd66bfbff3ea0727ef251eaec390a
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
      ./command sha256:ac3514b5830ee98bc181b15f5d92a3d3ddc8fba044572d1eb62083f1dc15d9b0
    ? ./command
}
