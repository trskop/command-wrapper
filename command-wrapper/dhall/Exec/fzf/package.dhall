-- vim: filetype=dhall

{ Options =
      ./Options/package.dhall sha256:58337b575c662f1e75e640b43fb29d295f396acfb6a77ecc99a09f3033b3fdb4
    ? ./Options/package.dhall
, command =
      ./command.dhall sha256:99bab6b9b93c0bf567e897f83b9d381eecf76cd35800e6ee09d420ed9857c5b0
    ? ./command.dhall
}
