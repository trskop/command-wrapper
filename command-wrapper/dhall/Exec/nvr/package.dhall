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
, completion-script =
      ./completion-script sha256:699491f149108814be8d08f8c23263fb40de7572df821482b4fd3d8f94fcdd85
    ? ./completion-script
, completion =
      ./completion sha256:615309adeb384974c4c697fb32de0fef1e3258ce562348e10798b6e0c18cd73d
    ? ./completion
}
