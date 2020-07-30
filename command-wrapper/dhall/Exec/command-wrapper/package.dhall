-- vim: filetype=dhall

{ command =
      ./command.dhall sha256:838f0153766e3994d1ec1899419781e925fd7e142515723c756938f5ae7cef1c
    ? ./command.dhall
, completion =
      ./completion.dhall sha256:b3c9eef9d09a402aed46f2482954c8281bd6e03ce1203ad07958d0daa452bfe7
    ? ./completion.dhall
, Options =
      ./Options/package.dhall sha256:5c50274c1e8693b4424d10d7668f5775d098810393e0d37354282f761d2483da
    ? ./Options/package.dhall
}
