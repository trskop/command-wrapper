-- vim: filetype=dhall

{ GlobalOptions =
      ./GlobalOptions/package.dhall sha256:832e6aa1216d8894c28bf5c94a2ae20bc4f24afe4439fe3e741a3e1fc7446724
    ? ./GlobalOptions/package.dhall
, DownOptions =
      ./DownOptions/package.dhall sha256:156b4f64a4150904b5a0fe59358cf9fcbf54bb37b1a8ba168a1faf616a8ee97e
    ? ./DownOptions/package.dhall
, UpOptions =
      ./UpOptions/package.dhall sha256:511cc4c23637c43a972ee8b4705655995bf5c9799682a0285da3c0e9cb445a34
    ? ./UpOptions/package.dhall
, Action =
      ./Action/package.dhall sha256:8032d2671d84019fea6f6d0bd55e9758780b321efee4324389b307e7f362234a
    ? ./Action/package.dhall
, command =
      ./command sha256:86d86d52c1889e70040ee0428ccd2abd4d0bcea7353544bf81e7db90ca1a704a
    ? ./command
, completion =
      ./completion sha256:61756e7dab90b23f29425cbf6412fa2a29009bfb603e5dc1890e361b96d793c1
    ? ./completion
, completion-script =
      ./completion-script sha256:79a6f96409191b948959c757703b94c3ebbb7cf5c86a013c769e23fc84bc7fd7
    ? ./completion-script
}
