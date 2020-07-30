-- vim: filetype=dhall

let EnvironmentVariable =
        ../../../CommandWrapper/EnvironmentVariable/Type sha256:b8c3c0c4ceb36ba4e6674df5de20ad1d97e120b93b9ce9914a41d0036770dcc4
      ? ../../../CommandWrapper/EnvironmentVariable/Type

let Environment =
        ./Type.dhall sha256:bfc2cb080bb0cac5a42a81beb707437389fa9c6f8e54ae8dd7ce09c6566140ee
      ? ./Type.dhall

let noArguments = [] : List Text

let toArguments =
      λ(environment : Environment) →
        List/fold
          EnvironmentVariable
          environment
          (List Text)
          ( λ(var : EnvironmentVariable) →
            λ(accum : List Text) →
              [ "--env", "${var.name}=${var.value}" ] # accum
          )
          noArguments

let test0 = assert : toArguments ([] : List EnvironmentVariable) ≡ noArguments

let test1 =
        assert
      :   toArguments
            [ { name = "CONFIG", value = "{hello = \"config\"}" }
            , { name = "PORT", value = "1234" }
            ]
        ≡ [ "--env", "CONFIG={hello = \"config\"}", "--env", "PORT=1234" ]

in  toArguments : Environment → List Text
