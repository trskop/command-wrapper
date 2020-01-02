-- vim: filetype=dhall

{ Options =
      ./Options/package.dhall sha256:f1e7a2f90b07fa3071a6c1720400054748d7a99a7f31cd1d6baca2d5cf62751b
    ? ./Options/package.dhall
, Action =
      ./Action/package.dhall sha256:d84b25ce7e2fc88391118a4bdb453d7c5af0d360262f54c1969e91e333fc340c
    ? ./Action/package.dhall
, command =
      ./command sha256:53005a100db768b88ca790522c4dc58be2c7022b5b64db52a62e1d2f8b6941d9
    ? ./command
}
