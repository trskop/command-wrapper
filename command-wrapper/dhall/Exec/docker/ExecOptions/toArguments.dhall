-- vim: filetype=dhall

let ExecOptions =
      { Type =
            ./Type.dhall sha256:f060f3aa27210e24ea2ff0fd222b3c0d4c376f79077d376c3b40596ad7d3d74a
          ? ./Type.dhall
      , default =
            ./default.dhall sha256:ecfa3148efb09aa997b99ed97df7f7f1e0184e251b56d9c5407021d35d008fdd
          ? ./default.dhall
      }

let Environment =
      { toArguments =
            ../Environment/toArguments.dhall sha256:d0426badaef8cd6535378bbd9b5caf073e2b7d794eae541a454ebcba4cb52531
          ? ../Environment/toArguments.dhall
      }

let optionalOptions =
        ../../utils/optionalOptions.dhall sha256:51a1fbb37ceddab8f0ff0ef0fe2cd910af960bd447c6bcbc01b23426c0ee74f8
      ? ../../utils/optionalOptions.dhall

let noArguments = [] : List Text

let toArguments =
      λ(execOptions : ExecOptions.Type) →
          (if execOptions.interactive then [ "--interactive" ] else noArguments)
        # (if execOptions.allocateTty then [ "--tty" ] else noArguments)
        # (if execOptions.detach then [ "--detach" ] else noArguments)
        # optionalOptions
            Text
            (λ(user : Text) → [ "--user", user ])
            execOptions.user
        # optionalOptions
            Text
            (λ(dir : Text) → [ "--workdir", dir ])
            execOptions.workingDirectory
        # optionalOptions
            Text
            (λ(file : Text) → [ "--env-file", file ])
            execOptions.environmentFile
        # Environment.toArguments execOptions.environment

let test0 = assert : toArguments ExecOptions::{=} ≡ noArguments

let test1 =
        assert
      :   toArguments
            ExecOptions::{
            , interactive = True
            , allocateTty = True
            , detach = True
            , user = Some "john"
            , workingDirectory = Some "/data"
            , environmentFile = Some "production.env"
            , environment =
              [ { name = "CONFIG", value = "{hello = \"service\"}" }
              , { name = "PORT", value = "1234" }
              ]
            }
        ≡ [ "--interactive"
          , "--tty"
          , "--detach"
          , "--user"
          , "john"
          , "--workdir"
          , "/data"
          , "--env-file"
          , "production.env"
          , "--env"
          , "CONFIG={hello = \"service\"}"
          , "--env"
          , "PORT=1234"
          ]

in  toArguments : ExecOptions.Type → List Text
