-- vim: filetype=dhall

{ Type =
      ./Type.dhall sha256:054af9262f73048f90af26b55ff29e9d6cb0b4b7f8002bdb1994495636f4cc84
    ? ./Type.dhall
, default =
      ./default.dhall sha256:430f666c22a7337f0f016178b33a11479041b5bf168ed9779703b074d630eb99
    ? ./default.dhall
, toArguments =
      ./toArguments.dhall sha256:44011c82c619ad18897156096d65fe26362bc2653c94518300d8453de721b0ec
    ? ./toArguments.dhall
, toEnvironment =
      ./toEnvironment.dhall sha256:ff5b3e81adc9bd518790ee80f9dc4f0aaac00f2dfe412ab567a79e963e998b7d
    ? ./toEnvironment.dhall
}
