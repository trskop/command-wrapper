-- vim: filetype=dhall

{ Type =
      ./Type.dhall sha256:7cbb3d15d2d57539fe0274c8c1223148bb7b34d0310e0c58d4ab7f49e7e0ca5f
    ? ./Type.dhall
, default =
      ./default.dhall sha256:fcc7c0bfd22e1c1cc54cfe6249340f56410e296a90da6720156af78b7b7365ca
    ? ./default.dhall
, toArguments =
      ./toArguments.dhall sha256:776f5fc80945990bf14784eb6840e6086163a4c4430b07361fa359e1521a22a9
    ? ./toArguments.dhall
, toEnvironment =
      ./toEnvironment.dhall sha256:8405c3d234dbdaaf6a7eaa56c2370710207881560933822ec7958088b1101959
    ? ./toEnvironment.dhall
}
