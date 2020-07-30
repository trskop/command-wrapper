-- vim: filetype=dhall

{ Type =
      ./Type.dhall sha256:54460863b32e729cf617282abfc10164e5ac65565bb8b71099b27567e6404a3d
    ? ./Type.dhall
, default =
      ./default.dhall sha256:e7779cc66f4072ec1586cc99158ee19641c282c3c03d6d42363876695c5af8ec
    ? ./default.dhall
, toArguments =
      ./toArguments.dhall sha256:67b247e0c720e969929ecacf4495c93deddb63d7b5c219ffa051941f7159ec90
    ? ./toArguments.dhall
, toEnvironment =
      ./toEnvironment.dhall sha256:7a056d025a1fadf0e2a2ff018a04922b99ffe6d58ef62248479e74a29831b5e3
    ? ./toEnvironment.dhall
}
