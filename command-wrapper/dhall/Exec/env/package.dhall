-- vim: filetype=dhall

{ Options =
      ./Options/package.dhall sha256:3ca1c4384ff0beb90e4d417cd4eeff1b8a6cf655bd0307f473d57b3c956d4007
    ? ./Options/package.dhall
, command =
      ./command sha256:05ecf234e66ec1efb02630906a2b3f6ec910d17b2b57911c6d997d82e7e6fa0e
    ? ./command
}
