-- vim: filetype=dhall

{ Type =
      ./Type.dhall sha256:61553461550695b5b8c52861fff9ceecf0c70ebd703cfa2f7465d97aca6d3a66
    ? ./Type.dhall
, default =
      ./default.dhall sha256:e105968e9739efb82a2a48c7bf6e15949e1d0b511dca30e2e2deb5a16e76a1fb
    ? ./default.dhall
, toArguments =
      ./toArguments.dhall sha256:01d31b1faf4bf9e23905ef2cd0dc35eaaecfc21431015bfe15a64efbe33d098e
    ? ./toArguments.dhall
, toEnvironment =
      ./toEnvironment.dhall sha256:ad6d2ee0b5c0f0dc3fc6e1c7b27257de09a99c8ec164bf57907ab634a8dfa9e9
    ? ./toEnvironment.dhall
}
