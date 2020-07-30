-- vim: filetype=dhall

let RunOptions =
      { Type =
            ./Type.dhall sha256:c43df7ef522e86341c59087914011ef5deadeba6b9363665a63ba5e15aa3647f
          ? ./Type.dhall
      , default =
            ./default.dhall sha256:44ddaa795338e389c4ec7ffdc4b43e6581e4718913f40408c0ada14a6a2179d0
          ? ./default.dhall
      }

let ExecOptions =
      { toArguments =
            ../ExecOptions/toArguments.dhall sha256:6537d30b5f6d9aa6bcd5526659273deb224f4296cdc565b0b3d948b92d67e1b2
          ? ../ExecOptions/toArguments.dhall
      }

let noArguments = [] : List Text

let toArguments =
      λ(runOptions : RunOptions.Type) →
          ExecOptions.toArguments
            runOptions.{ interactive
                       , allocateTty
                       , detach
                       , user
                       , workingDirectory
                       , environment
                       , environmentFile
                       }
        # (if runOptions.remove then [ "--rm" ] else noArguments)

let test0 = assert : toArguments RunOptions::{=} ≡ noArguments

let test1 =
        assert
      :   toArguments RunOptions::{ interactive = True, remove = True }
        ≡ [ "--interactive", "--rm" ]

in  toArguments : RunOptions.Type → List Text
