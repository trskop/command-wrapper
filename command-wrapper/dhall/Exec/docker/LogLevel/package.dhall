-- vim: filetype=dhall

{ Type =
      ./Type.dhall sha256:f63d8d8d8c27e7edd9b623e5abea25b28f94293d2461347c718782ae09dfc41b
    ? ./Type.dhall
, toText =
      ./toText.dhall sha256:036de01455477bda370a28fcd8cee650aef6b8ee307dd8cbeb7a5663a73611f7
    ? ./toText.dhall
, toArguments =
      ./toArguments.dhall sha256:75671e73a9221bb136ff836da51f9f299f6fbea469583ca49519682737cdda89
    ? ./toArguments.dhall
}
