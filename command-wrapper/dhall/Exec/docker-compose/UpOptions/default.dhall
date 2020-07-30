-- vim: filetype=dhall

let UpOptions =
      { Type =
            ./Type.dhall sha256:ef326d47d9073f2c4b0ce34dfc14ea188551fac2170499087cbfacf2d5a73073
          ? ./Type.dhall
      }

let default = { noColour = False, buildImages = None Bool, detach = False }

let consistency = assert : (UpOptions ⫽ { default })::{=} ≡ default

in  default
