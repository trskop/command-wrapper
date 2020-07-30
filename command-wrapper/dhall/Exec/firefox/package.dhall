-- vim: filetype=dhall

{ Options =
      ./Options/package.dhall sha256:664ed84104a3732626779e79d3b1521eb554aff1b9ef09070d2d2d2d6ac5ee18
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
      ./command.dhall sha256:5bdb1bde7d909c700debd52d62e10a24daf920ff7355f16cffa8483982077cfb
    ? ./command.dhall
}
