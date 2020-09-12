-- vim: filetype=dhall

let Options =
      { Type =
            ./Type.dhall sha256:d1c9b4ea8ea803d299311283186f7c1c635dc05c8b8572dbf67dd467009d024d
          ? ./Type.dhall
      , default =
            ./default.dhall sha256:fa59f7335ad8b2e11a4f3bdb4987e96379dca2797c3498e4d48db40066f6ecb5
          ? ./default.dhall
      }

let EnvironmentVariable =
        ../../../CommandWrapper/EnvironmentVariable/package.dhall sha256:1250426124fe1a06fce13386a45e803655f7fe419fb372bd4a5157993350b199
      ? ../../../CommandWrapper/EnvironmentVariable/package.dhall

let Prelude =
        ../../../Prelude/package.dhall sha256:2086c6a2e7ce8ee371858036ab60d24d045969defadb4bc9d87639921ec2e028
      ? ../../../Prelude/package.dhall

let noArguments = [] : List Text

let toArguments =
      λ(options : Options.Type) →
          (if options.ignoreEnvironment then [ "-i" ] else noArguments)
        # Prelude.List.concatMap
            Text
            Text
            (λ(name : Text) → [ "-u", name ])
            options.undefine
        # Prelude.List.concatMap
            EnvironmentVariable.Type
            Text
            ( λ(var : EnvironmentVariable.Type) →
                [ EnvironmentVariable.toText var ]
            )
            options.define

let test0 = assert : toArguments Options::{=} ≡ noArguments

let test1 =
        assert
      :   toArguments
            Options::{
            , ignoreEnvironment = True
            , undefine = [ "FOO" ]
            , define = [ { name = "BAR", value = "baz" } ]
            }
        ≡ [ "-i", "-u", "FOO", "BAR=baz" ]

in  toArguments : Options.Type → List Text
