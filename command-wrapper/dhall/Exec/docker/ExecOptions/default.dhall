-- vim: filetype=dhall

let ExecOptions =
      { Type =
            ./Type.dhall sha256:f060f3aa27210e24ea2ff0fd222b3c0d4c376f79077d376c3b40596ad7d3d74a
          ? ./Type.dhall
      }

let Environment =
      { empty =
            ../Environment/empty.dhall sha256:cd98ff0deea70057d59baf060c91fba887fc149198b7998ba6581a08c4793a6e
          ? ../Environment/empty.dhall
      }

let default =
      { interactive = False
      , allocateTty = False
      , detach = False
      , user = None Text
      , workingDirectory = None Text
      , environment = Environment.empty
      , environmentFile = None Text
      }

let consistency = assert : (ExecOptions ∧ { default })::{=} ≡ default

in  default
