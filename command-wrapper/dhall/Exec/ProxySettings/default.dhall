-- vim: filetype=dhall

let ProxySettings =
      { Type =
            ./Type.dhall sha256:6ab5e997b4b51ec84ec2c002c400f173d60e3863e088bf2c2b92028a39196825
          ? ./Type.dhall
      }

let default =
      { httpProxy = None Text
      , httpsProxy = None Text
      , ftpProxy = None Text
      , rsyncProxy = None Text
      , allProxy = None Text
      , noProxy = [] : List Text
      }

let consistency = assert : (ProxySettings ⫽ { default })::{=} ≡ default

in  default
