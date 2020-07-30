-- vim: filetype=dhall

let UpOptions =
      { Type =
            ./Type.dhall sha256:ef326d47d9073f2c4b0ce34dfc14ea188551fac2170499087cbfacf2d5a73073
          ? ./Type.dhall
      , default =
            ./default.dhall sha256:666b5b167b3ee41c9aa63db7c6d8d44d96becaf67a94819bffc65ef48073a2dd
          ? ./default.dhall
      }

let optionalFlags =
        ../../utils/optionalFlags.dhall sha256:0dba774441dd92889f9a2a9819a6bca5ad7d1d891fbac9fa5c284367ca9fec33
      ? ../../utils/optionalFlags.dhall

let noArguments = [] : List Text

let toArguments =
      λ(options : UpOptions.Type) →
          optionalFlags [ "--build" ] [ "--no-build" ] options.buildImages
        # (if options.noColour then [ "--no-colour" ] else noArguments)
        # (if options.detach then [ "--detach" ] else noArguments)

let test0 = assert : toArguments UpOptions::{=} ≡ noArguments

let test1 =
        assert
      :   toArguments
            UpOptions::{
            , buildImages = Some True
            , noColour = True
            , detach = True
            }
        ≡ [ "--build", "--no-colour", "--detach" ]

in  toArguments : UpOptions.Type → List Text
