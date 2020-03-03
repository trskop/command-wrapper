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
      ./completion sha256:e5d99fc3a06764e3762b26a82b8037dc7464fa78b9de181497af1dc4d6b4faf2
    ? ./completion
, completion-script =
      ./completion-script sha256:79a6f96409191b948959c757703b94c3ebbb7cf5c86a013c769e23fc84bc7fd7
    ? ./completion-script
}
