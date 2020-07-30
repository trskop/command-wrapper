-- vim: filetype=dhall

{ Options =
      ./Options/package.dhall sha256:9d69767722530395eb1abf5b5f47f84ee7db88864978032b33098ee6b030c44f
    ? ./Options/package.dhall
, Action =
      ./Action/package.dhall sha256:8fe003e9f0c71f1e1f0c9d011864c217c0f050025da88414292b8394f9d00f8d
    ? ./Action/package.dhall
, command =
      ./command.dhall sha256:5b1f41d409feaec6d44bd0817e3e7e5eff560db30ea503ac6860c0814feddee9
    ? ./command.dhall
}
