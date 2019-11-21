-- vim: filetype=dhall

{ GlobalOptions =
      ./GlobalOptions/package.dhall sha256:832e6aa1216d8894c28bf5c94a2ae20bc4f24afe4439fe3e741a3e1fc7446724
    ? ./GlobalOptions/package.dhall
, DownOptions =
      ./DownOptions/package.dhall sha256:cd6a167e028869ab5b1ef467d1e9fbe553aae0026ed08fba550ea2fb50a59db6
    ? ./DownOptions/package.dhall
, UpOptions =
      ./UpOptions/package.dhall sha256:6914f832dc6a005b882d8ca4df89a5ffa0e4112f6678539790a955892806f1f8
    ? ./UpOptions/package.dhall
, Action =
      ./Action/package.dhall sha256:8032d2671d84019fea6f6d0bd55e9758780b321efee4324389b307e7f362234a
    ? ./Action/package.dhall
, command =
      ./command sha256:93352cea1e6fc63905cfa717365d50324f427e2cd5df63a91c9af00c084e927e
    ? ./command
, completion =
      ./completion sha256:09ee608f3cbb8957519d2edf3b693953cab74b4eefcc708c6bdf34ad36a2daf2
    ? ./completion
}
