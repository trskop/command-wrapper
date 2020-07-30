-- vim: filetype=dhall

{ Options =
      ./Options/package.dhall sha256:aa82f4fd081fde40cd8fc09a406a2064062bb566306e2d0ca520c28403b9b634
    ? ./Options/package.dhall
, Action =
      ./Action/package.dhall sha256:d84b25ce7e2fc88391118a4bdb453d7c5af0d360262f54c1969e91e333fc340c
    ? ./Action/package.dhall
, command =
      ./command.dhall sha256:808380ecce9cbaaa658d97706c19fc51e91a6e9c36931366d18663e133332a6b
    ? ./command.dhall
, completion-script =
      ./completion-script.dhall sha256:699491f149108814be8d08f8c23263fb40de7572df821482b4fd3d8f94fcdd85
    ? ./completion-script.dhall
, completion =
      ./completion.dhall sha256:225c5714ee91d346022a7de72a62cd286538c8e70095a2ed8de38ac3b4843a8a
    ? ./completion.dhall
}
