-- vim: filetype=dhall
--
-- These are some known terminal emulators that support setting working
-- directory via command line option.

{ Type =
      ./Type sha256:0801c7f67dd1a47360ee4727176658af658d850aed28a3c3832ad279067f994a
    ? ./Type
, gnome-terminal =
      ./gnome-terminal sha256:63ab557caff51470ed179f905d6c62de15008feb1b0288603432d9b512c4105b
    ? ./gnome-terminal
, kitty =
      ./kitty sha256:2fa5e611718be0eca0898f626ec16015f2d40d2b0959b0d62420ee6d01f385dd
    ? ./kitty
, urxvt =
      ./urxvt sha256:9b60c0f6e641afde980d307d986d29ae32c593d4af50b31b8ea5a20cf21eb1e2
    ? ./urxvt
}
