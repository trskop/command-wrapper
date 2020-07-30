-- vim: filetype=dhall

let Options =
      { Type =
            ./Type.dhall sha256:dbef3a3dd77c5377d0f571fde20161110ea5f99383ee7ac3c97cdee6b72cab67
          ? ./Type.dhall
      , default =
            ./default.dhall sha256:a6920a5071e8cd8468202daf08c7ea5727803a98dc83311a27265cfb7a351041
          ? ./default.dhall
      }

let LogLevel =
        ../LogLevel/package.dhall sha256:f47b621e6f3d257ff2037ef1b59e1155b8c7f569f371b285ae8bf8169adff76d
      ? ../LogLevel/package.dhall

let optionalOptions =
        ../../utils/optionalOptions.dhall sha256:51a1fbb37ceddab8f0ff0ef0fe2cd910af960bd447c6bcbc01b23426c0ee74f8
      ? ../../utils/optionalOptions.dhall

let noArguments = [] : List Text

let toArguments =
      λ(options : Options.Type) →
          List/fold
            Text
            options.host
            (List Text)
            ( λ(host : Text) →
              λ(options : List Text) →
                [ "--host", host ] # options
            )
            noArguments
        # optionalOptions LogLevel.Type LogLevel.toArguments options.logLevel
        # optionalOptions
            Text
            (λ(config : Text) → [ "--config", config ])
            options.config
        # (if options.debug then [ "--debug" ] else noArguments)

let test0 = assert : toArguments Options::{=} ≡ noArguments

let test1 =
        assert
      :   toArguments
            Options::{
            , host = [ "localhost", "example.com" ]
            , logLevel = Some LogLevel.Type.info
            , config = Some "/home/user/.config/docker"
            }
        ≡ [ "--host"
          , "localhost"
          , "--host"
          , "example.com"
          , "--log-level"
          , "info"
          , "--config"
          , "/home/user/.config/docker"
          ]

in  toArguments : Options.Type → List Text
