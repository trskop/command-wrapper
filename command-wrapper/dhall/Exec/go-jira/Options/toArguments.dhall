-- vim: filetype=dhall

let Verbosity =
        ../../../CommandWrapper/Verbosity/Type sha256:f009a39a49b1ee65651e9510778e7d72ff96820f4702a955e8f47682d72995c6
      ? ../../../CommandWrapper/Verbosity/Type

let Verbosity/fold =
        ../../../CommandWrapper/Verbosity/fold sha256:4dac2c264a2531d569ad0e5f712a1cd2d17b51ecdc502cc72f19937bf4733b1e
      ? ../../../CommandWrapper/Verbosity/fold

let Options =
      { Type =
            ./Type.dhall sha256:89bafb720da9507168156fc176d0216d081875e79b1a23f8c301fdeb3721bb6d
          ? ./Type.dhall
      , default =
            ./default.dhall sha256:a551a4da0bcc26f8b40b3426cb5121c7fa56e3962686d78e7b787cd2c0c518e6
          ? ./default.dhall
      }

let optionalOptions =
        ../../utils/optionalOptions.dhall sha256:51a1fbb37ceddab8f0ff0ef0fe2cd910af960bd447c6bcbc01b23426c0ee74f8
      ? ../../utils/optionalOptions.dhall

let noArguments = [] : List Text

let toArguments =
      λ(options : Options.Type) →
      λ(verbosity : Optional Verbosity) →
          [ "--endpoint=${options.baseUrl}"
          , "--login=${options.login}"
          , "--user=${options.user}"
          ]
        # optionalOptions
            Verbosity
            ( Verbosity/fold
                (List Text)
                { Annoying = [ "-vvv" ]
                , Normal = noArguments
                , Silent = [ "--quiet" ]
                , Verbose = [ "--verbose" ]
                }
            )
            verbosity

let test0 =
        assert
      :   toArguments
            Options::{
            , baseUrl = "https://example.com/v1"
            , login = "user@example.com"
            , user = "user"
            , token = "token"
            }
            (None Verbosity)
        ≡ [ "--endpoint=https://example.com/v1"
          , "--login=user@example.com"
          , "--user=user"
          ]

let test1 =
        assert
      :   toArguments
            Options::{
            , baseUrl = "https://example.com/v1"
            , login = "user@example.com"
            , user = "user"
            , token = "token"
            }
            (Some Verbosity.Annoying)
        ≡ [ "--endpoint=https://example.com/v1"
          , "--login=user@example.com"
          , "--user=user"
          , "-vvv"
          ]

in  toArguments : Options.Type → Optional Verbosity → List Text
