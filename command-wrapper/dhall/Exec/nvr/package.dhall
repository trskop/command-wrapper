-- vim: filetype=dhall

{ Options =
      ./Options/package.dhall sha256:3fc817638aad103c940a0c4fb2c2a3b4dbe80d14100bb99ba94c75191664d466
    ? ./Options/package.dhall
, Action =
      ./Action/package.dhall sha256:d84b25ce7e2fc88391118a4bdb453d7c5af0d360262f54c1969e91e333fc340c
    ? ./Action/package.dhall
, command =
      ./command sha256:2a6fe7db02e1c794c74675c951a4ec0f424bf247960cb7ab1707f874bbc55878
    ? ./command
, completion-script =
      ./completion-script sha256:699491f149108814be8d08f8c23263fb40de7572df821482b4fd3d8f94fcdd85
    ? ./completion-script
, completion =
      ./completion sha256:d23d3b998c5b205b41bc9cdec00107f9c9f6aa7169470ce7541614d8d5f8d8f7
    ? ./completion
}
