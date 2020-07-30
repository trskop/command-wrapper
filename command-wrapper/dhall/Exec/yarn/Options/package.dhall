-- vim: filetype=dhall

{ Type =
      ./Type.dhall sha256:4a4dda0eaf771745c5bfdefb80cdf489c87385626c9501ee19e00485bca4b48f
    ? ./Type.dhall
, default =
      ./default.dhall sha256:e5843167eab527a6c37c556528ec5ffedb1ba0bf2f0e5f39001b1b1718a3cf1d
    ? ./default.dhall
, toArguments =
      ./toArguments.dhall sha256:448141ce4f4b04d29fd59087dd5e2a83245800b2fd92913ac3b4afbed6c62cf8
    ? ./toArguments.dhall
, toEnvironment =
      ./toEnvironment.dhall sha256:9fce7b8fa1e76b243a11d27c802fcb33d5b658e369cd8185d0778358592e4a30
    ? ./toEnvironment.dhall
}
