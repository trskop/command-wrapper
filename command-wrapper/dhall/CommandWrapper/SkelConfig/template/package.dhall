-- vim: filetype=dhall

{ bash =
      ./bash.dhall sha256:ea187c51f5cd991a2ac221519a1581e635049424e8c91696cd699d952f5c3555
    ? ./bash.dhall
, dhall =
      ./dhall.dhall sha256:077ad60117d391fc17bc0d374afaa0542be2bda112be1cba14a937981e097e99
    ? ./dhall.dhall
, haskell =
      ./haskell.dhall sha256:834ed5c4fa401bebb113d082a88d1b6479041cc1573467b58b251ced86493d1d
    ? ./haskell.dhall
}
