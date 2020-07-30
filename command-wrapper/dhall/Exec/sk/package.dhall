-- vim: filetype=dhall

{ Options =
      ./Options/package.dhall sha256:c9af3f4c7cf78a2c561aa04dd378187556591b5e04cf9c9e44cb11d7e823670b
    ? ./Options/package.dhall
, command =
      ./command.dhall sha256:f71b650012cf19e07eb5eb8d0d9afd70f24d7e2e1b164c421599e6323163b709
    ? ./command.dhall
}
