-- vim: filetype=dhall

let Options =
      { Type =
            ./Type.dhall sha256:82c559b2efa14b05efa1644a6ac53852c45823fd1395f1aa223fd8f9aa549b5c
          ? ./Type.dhall
      }

let LogLevel =
        ../LogLevel/Type.dhall sha256:653f22f10a9c744a6b718ddd98eb1c166d430edf82d5e201b971e798c52b5210
      ? ../LogLevel/Type.dhall

let CommonOptions =
      { default =
            ../../CommonOptions/default.dhall sha256:a551a4da0bcc26f8b40b3426cb5121c7fa56e3962686d78e7b787cd2c0c518e6
          ? ../../CommonOptions/default.dhall
      }

let default =
        CommonOptions.default
      ∧ { files = [] : List Text
        , projectName = None Text
        , projectDirectory = None Text
        , contextName = None Text
        , environmentFile = None Text
        , logLevel = None LogLevel
        , noAnsi = False
        }

let consistency = assert : (Options ∧ { default })::{=} ≡ default

in  default
