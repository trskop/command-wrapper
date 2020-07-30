-- vim: filetype=dhall

{ Type =
      ./Type.dhall sha256:888713faabcc80155c73e123229956496a179aeb6737d9c460107ab0b59c373f
    ? ./Type.dhall
, default =
      ./default.dhall sha256:b81b593542b28171d9342252efa35f2d9b78625fefc073efb14be22c2e175454
    ? ./default.dhall
, toArguments =
      ./toArguments.dhall sha256:fa280e851fa4c2ee59d24cddd7b999922026ed7f5ebed28a965b77e9cc3be2e5
    ? ./toArguments.dhall
, toEnvironment =
      ./toEnvironment.dhall sha256:721b4304bfbffa12d38cde308d54bc96348e9820101f22fef487bac777af95bb
    ? ./toEnvironment.dhall
}
