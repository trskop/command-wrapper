-- vim: filetype=dhall

let DownOptions =
        ../DownOptions/Type.dhall sha256:760e4cd0b9b14aa5c3225f155da55eb8eef9c41a75a4535665ac5d7c6aceb182
      ? ../DownOptions/Type.dhall

let UpOptions =
        ../UpOptions/Type.dhall sha256:ef326d47d9073f2c4b0ce34dfc14ea188551fac2170499087cbfacf2d5a73073
      ? ../UpOptions/Type.dhall

in  < Down : DownOptions | Up : UpOptions >
