-- vim: filetype=dhall

{ Type =
      ./Type.dhall sha256:760e4cd0b9b14aa5c3225f155da55eb8eef9c41a75a4535665ac5d7c6aceb182
    ? ./Type.dhall
, default =
      ./default.dhall sha256:3ac386ca465801038a423b857211761ad708aacaefe514b0d9824e4d819f2633
    ? ./default.dhall
, toArguments =
      ./toArguments.dhall sha256:b617689d9b96bb63ebac9a915ab2f8bcfa3f270830602d6fc90f3834892b658c
    ? ./toArguments.dhall
}
