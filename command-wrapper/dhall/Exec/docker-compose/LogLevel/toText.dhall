-- vim: filetype=dhall
--
-- Convert `LogLevel` into a value accepted by `--log-level=LEVEL` global option
-- of `docker-compose`

let LogLevel =
        ./Type.dhall sha256:653f22f10a9c744a6b718ddd98eb1c166d430edf82d5e201b971e798c52b5210
      ? ./Type.dhall

let toText =
      λ(_ : LogLevel) →
        merge
          { Debug = "DEBUG"
          , Info = "INFO"
          , Warning = "WARNING"
          , Error = "ERROR"
          , Critical = "CRITICAL"
          }
          _

let test0 = assert : toText LogLevel.Debug ≡ "DEBUG"

in  toText : LogLevel → Text
