-- vim: filetype=dhall

let Shell =
        ../../CommandWrapper/Shell/Type sha256:f61ef033bfb850ef4bf0c3d0d18c69d1b15b38cc9e3df4a1abea334b89ed5555
      ? ../../CommandWrapper/Shell/Type

let optparse-applicative =
        ../completion/optparse-applicative.dhall sha256:00d877d341515117d8f20b93d7119bd0342ece43d2202e01f470db73f162cb44
      ? ../completion/optparse-applicative.dhall

let Options =
      { Type =
            ./Options/Type.dhall sha256:b2526c288d5a0419d362c1375b879214396f6d1f6cd88b0416545724fd661b26
          ? ./Options/Type.dhall
      , toArguments =
            ./Options/toArguments.dhall sha256:378f10f92fda1dedf67914020e2bd332e1fff495b9a32dc15a1a01ef0e82a30b
          ? ./Options/toArguments.dhall
      }

let completion =
      λ(options : Options.Type) →
      λ(arguments : List Text) →
      λ(shell : Shell) →
      λ(index : Natural) →
      λ(words : List Text) →
        let completionCommand =
              optparse-applicative
                "stack"
                (Options.toArguments options # arguments)
                shell
                index
                words

        in    completionCommand
            ⫽ { workingDirectory =
                  merge
                    { None = completionCommand.workingDirectory
                    , Some = λ(_ : Text) → Some _
                    }
                    options.workingDirectory
              }

in  completion
