-- vim: filetype=dhall

{ Type =
      ./Type.dhall sha256:e908cbddb6ec0b95462e513b70eaf6181aed7908212ad1f5614ee736bc174c9c
    ? ./Type.dhall
, default =
      ./default.dhall sha256:ce42936b737e0d7ccd23c499b1484179bd083c5a4f31749ab46aed2402cd1adc
    ? ./default.dhall
, toArguments =
      ./toArguments.dhall sha256:2cd11f648fce2576f14f88a4026f87af8a50f20b274b9489380234a935f9d6ba
    ? ./toArguments.dhall
, toEnvironment =
      ./toEnvironment.dhall sha256:b091c450f2f169a972dee5692f839c7bd34b69979d9c989663d195a7e0dd85fb
    ? ./toEnvironment.dhall
}
