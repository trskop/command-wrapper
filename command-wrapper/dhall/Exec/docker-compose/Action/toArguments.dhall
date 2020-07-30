-- vim: filetype=dhall

let DownOptions =
      { Type =
            ../DownOptions/Type.dhall sha256:760e4cd0b9b14aa5c3225f155da55eb8eef9c41a75a4535665ac5d7c6aceb182
          ? ../DownOptions/Type.dhall
      , default =
            ../DownOptions/default.dhall sha256:3ac386ca465801038a423b857211761ad708aacaefe514b0d9824e4d819f2633
          ? ../DownOptions/default.dhall
      , toArguments =
            ../DownOptions/toArguments.dhall sha256:b617689d9b96bb63ebac9a915ab2f8bcfa3f270830602d6fc90f3834892b658c
          ? ../DownOptions/toArguments.dhall
      }

let UpOptions =
      { Type =
            ../UpOptions/Type.dhall sha256:ef326d47d9073f2c4b0ce34dfc14ea188551fac2170499087cbfacf2d5a73073
          ? ../UpOptions/Type.dhall
      , default =
            ../UpOptions/default.dhall sha256:666b5b167b3ee41c9aa63db7c6d8d44d96becaf67a94819bffc65ef48073a2dd
          ? ../UpOptions/default.dhall
      , toArguments =
            ../UpOptions/toArguments.dhall sha256:97e6ac449b50162a0fc65959acb43792c85175065f24932585d110e4d8957ebe
          ? ../UpOptions/toArguments.dhall
      }

let Action =
        ./Type.dhall sha256:e88f3cc3042912a2d9a24b75f93865227edbf368a70f5a8bb10f137abc8063c2
      ? ./Type.dhall

let toArguments =
      λ(_ : Action) →
        merge
          { Down =
              λ(_ : DownOptions.Type) → [ "down" ] # DownOptions.toArguments _
          , Up = λ(_ : UpOptions.Type) → [ "up" ] # UpOptions.toArguments _
          }
          _

let test0 = assert : toArguments (Action.Down DownOptions::{=}) ≡ [ "down" ]

let test1 = assert : toArguments (Action.Up UpOptions::{=}) ≡ [ "up" ]

let test2 =
        assert
      :   toArguments
            (Action.Down DownOptions::{ removeOrphanContainers = True })
        ≡ [ "down", "--remove-orphans" ]

let test3 =
        assert
      :   toArguments (Action.Up UpOptions::{ detach = True })
        ≡ [ "up", "--detach" ]

in  toArguments : Action → List Text
