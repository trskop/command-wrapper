-- vim: filetype=dhall

{ Type =
      ./Type.dhall sha256:d10faab8c8388e68d0e931248dee4741cbcef8b593095d8dd8a42a54c2272ab5
    ? ./Type.dhall
, default =
      ./default.dhall sha256:6bcdf525021173128fe71180d9f1a2e99cf514d0ab07bf391693d107e846125f
    ? ./default.dhall
, toEnvironment =
      ./toEnvironment.dhall sha256:660fd2452fe3c69253efb6b72e7f5b19e27dcdd13db8cb456d9d0579d20944ea
    ? ./toEnvironment.dhall
, toArguments =
      ./toArguments.dhall sha256:dc15673ba58fb5d66d1fe8ed53ed3ec2c124c8cb0ed6be4b75fa6285f2bc841c
    ? ./toArguments.dhall
}
