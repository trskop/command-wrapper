-- vim: filetype=dhall

{ Type =
      ./Type.dhall sha256:82c559b2efa14b05efa1644a6ac53852c45823fd1395f1aa223fd8f9aa549b5c
    ? ./Type.dhall
, default =
      ./default.dhall sha256:9045715df45ae4d2438d75ea7c934cb3e113716be3e88c52b34a18dfadb8a1a2
    ? ./default.dhall
, toArguments =
      ./toArguments.dhall sha256:902d7feef94fbc8bc2346e7ddbe21cdb3694421b555c4152ca8fc049917fe6ce
    ? ./toArguments.dhall
, toEnvironment =
      ./toEnvironment.dhall sha256:7d49ec6260c6e9b2bc0c5ccbcfdd5dc60f5e7171e03a22938cebaef376190cd8
    ? ./toEnvironment.dhall
}
