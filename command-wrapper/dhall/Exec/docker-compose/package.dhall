-- vim: filetype=dhall

{ Options =
      ./Options/package.dhall sha256:2c80a359ddb8d2b917793630aeb3371d8d2de96f7b5114bba82d4f0170163c4e
    ? ./Options/package.dhall
, DownOptions =
      ./DownOptions/package.dhall sha256:156b4f64a4150904b5a0fe59358cf9fcbf54bb37b1a8ba168a1faf616a8ee97e
    ? ./DownOptions/package.dhall
, UpOptions =
      ./UpOptions/package.dhall sha256:511cc4c23637c43a972ee8b4705655995bf5c9799682a0285da3c0e9cb445a34
    ? ./UpOptions/package.dhall
, Action =
      ./Action/package.dhall sha256:c5cd737d9d99aff2dcb3482f1e9082b4db141d4cd04ca5e81f74fe1f4f7a0627
    ? ./Action/package.dhall
, LogLevel =
      ./LogLevel/package.dhall sha256:47286c7e3a465280e6e4994c0272e87b386b2638088b4c2960d89b032870968a
    ? ./LogLevel/package.dhall
, command =
      ./command.dhall sha256:470792b108a8fbb73d983743e443a32192dfeb779e38bbc823d04e171997fc75
    ? ./command.dhall
, completion =
      ./completion.dhall sha256:0ec45c44ffb77db786d093e4072728a5f6bcdeae3af2f9221306eee4cacbfe81
    ? ./completion.dhall
, completion-script =
      ./completion-script.dhall sha256:79a6f96409191b948959c757703b94c3ebbb7cf5c86a013c769e23fc84bc7fd7
    ? ./completion-script.dhall
}
