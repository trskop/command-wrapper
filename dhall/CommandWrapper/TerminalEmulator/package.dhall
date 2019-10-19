-- vim: filetype=dhall
--
-- These are some known terminal emulators that support setting working
-- directory via command line option.

{ Type =
      ./Type sha256:0801c7f67dd1a47360ee4727176658af658d850aed28a3c3832ad279067f994a
    ? ./Type
, gnome-terminal =
      ./gnome-terminal sha256:8541df930464fb40a2b098a58364303b3b39896c54b3f384bb53e27648330837
    ? ./gnome-terminal
, kitty =
      ./kitty sha256:8bd440fe1fb54754b67d384ba3d52f2dc38a5ccffe358e75b7d863a362aa7576
    ? ./kitty
, urxvt =
      ./urxvt sha256:a2a5f3204af9888a219f457ddf2040777564e3c420ee8e6554234ef08056fd91
    ? ./urxvt
}
