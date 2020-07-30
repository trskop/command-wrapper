-- vim: filetype=dhall

{ Type =
      ./Type.dhall sha256:9c93aeda7d25765472107935265ba78477d16d1787b33f0d6db56d27174bb532
    ? ./Type.dhall
, default =
      ./default.dhall sha256:49a9f1ae25424574c1694747822da1b8627010c47f18d495e197a9ca36c317b7
    ? ./default.dhall
, toArguments =
      ./toArguments.dhall sha256:4bebf3fbb475ac672bc913da31a5641744009b67c9c9bb3dad86e777f75c63d8
    ? ./toArguments.dhall
, toEnvironment =
      ./toEnvironment.dhall sha256:daa23cf9400a2ed8a8459c9c15d8da24f2af7ec74739aab9ba1008666751ee6d
    ? ./toEnvironment.dhall
}
