-- vim: filetype=dhall

{ Type =
      ./Type.dhall sha256:5c7701e806aeb7526088ef25f1233f2237342a4b1fd617a028f1258983d5202c
    ? ./Type.dhall
, default =
      ./default.dhall sha256:607cce48bb9e32e0d36aca288ca9b7aef0377d7c64f09f22e5445fd846782320
    ? ./default.dhall
, toArguments =
      ./toArguments.dhall sha256:c7b19e37a053afab7bf7dd27531f833958cb660298366b97ed858a36161c8c19
    ? ./toArguments.dhall
, toEnvironment =
      ./toEnvironment.dhall sha256:fa880d8d2d2d481f5f344b12e4617c0d034577c3fdf30b7b58fe922683428e8f
    ? ./toEnvironment.dhall
}
