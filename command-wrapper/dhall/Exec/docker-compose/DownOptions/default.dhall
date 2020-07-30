-- vim: filetype=dhall

let DownOptions =
      { Type =
            ./Type.dhall sha256:760e4cd0b9b14aa5c3225f155da55eb8eef9c41a75a4535665ac5d7c6aceb182
          ? ./Type.dhall
      }

let default =
      { removeImages = None < All | Local >
      , removeNamedVolumes = False
      , removeOrphanContainers = False
      }

let consistency = assert : (DownOptions ⫽ { default })::{=} ≡ default

in  default
