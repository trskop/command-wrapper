-- vim: filetype=dhall

{ Type =
      ./Type.dhall sha256:89bafb720da9507168156fc176d0216d081875e79b1a23f8c301fdeb3721bb6d
    ? ./Type.dhall
, toArguments =
      ./toArguments.dhall sha256:70cb6da8907898be8fca141df6695546fbb878cade8bd2559b144868f3a3338f
    ? ./toArguments.dhall
, toEnvironment =
      ./toEnvironment.dhall sha256:2ee7820542005468ab46f11a81b1a0cdbdd5731e7e26c4708f055bd19dd1bb14
    ? ./toEnvironment.dhall
}
