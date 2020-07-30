-- vim: filetype=dhall

{ Type =
      ./Type.dhall sha256:b583193040d153654a70a31291aa959845a9d3246c367e65567b3d96dd3c6730
    ? ./Type.dhall
, default =
      ./default.dhall sha256:c194361ef28684b8a55c18d6e9176f2b7c77878cb9f52a94cc568b70f9695f55
    ? ./default.dhall
, toArguments =
      ./toArguments.dhall sha256:fdc61532ffe4bc2826eaabdcbbe97571b5aa5aebe1e83a766332193ece99e877
    ? ./toArguments.dhall
, toEnvironment =
      ./toEnvironment.dhall sha256:e6a9c59f06e42f66e24b4d71595fefe8b35272de3da6152907df6b2b34ea9890
    ? ./toEnvironment.dhall
}
