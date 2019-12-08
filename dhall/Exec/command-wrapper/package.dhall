-- vim: filetype=dhall

{ command =
      ./command sha256:665cd45a07371af1d112faba2628568ce73cd33936d3a157f88e2772e6b219d5
    ? ./command
, completion =
      ./completion sha256:c9708e2b1ada0dad28a04835c9ced5e3026ea36cc200acbad6eb695041a9671b
    ? ./completion
, GlobalOptions =
      ./GlobalOptions/package.dhall sha256:9fd9054401b8aa5fdd448fb454ddc08e1d2c8696dd4560f7e7933af395fe0501
    ? ./GlobalOptions/package.dhall
}
