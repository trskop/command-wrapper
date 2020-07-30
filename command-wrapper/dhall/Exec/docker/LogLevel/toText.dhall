-- vim: filetype=dhall

let LogLevel =
        ./Type.dhall sha256:f63d8d8d8c27e7edd9b623e5abea25b28f94293d2461347c718782ae09dfc41b
      ? ./Type.dhall

let toText =
      λ(_ : LogLevel) →
        merge
          { debug = "debug"
          , info = "info"
          , warn = "warn"
          , error = "error"
          , fatal = "fatal"
          }
          _

let test0 = assert : toText LogLevel.info ≡ "info"

in  toText : LogLevel → Text
