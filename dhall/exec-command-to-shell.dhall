let ExecCommand = ./CommandWrapper/Type/ExecCommand

let EnvironmentVariable = ./CommandWrapper/Type/EnvironmentVariable

let List/map =
      https://prelude.dhall-lang.org/List/map sha256:dd845ffb4568d40327f2a817eb42d1c6138b929ca758d50bc33112ef3c885680

let Text/concatMapSep =
      https://prelude.dhall-lang.org/Text/concatMapSep sha256:c272aca80a607bc5963d1fcb38819e7e0d3e72ac4d02b1183b1afb6a91340840

let Text/concatSep =
      https://prelude.dhall-lang.org/Text/concatSep sha256:e4401d69918c61b92a4c0288f7d60a6560ca99726138ed8ebc58dca2cd205e58

let envVarToText = λ(var : EnvironmentVariable) → "${var.name}=${var.value}"

in    λ(cmd : ExecCommand)
    →     "env "
      ++  Text/concatMapSep
            " "
            Text
            Text/show
            (   List/map EnvironmentVariable Text envVarToText cmd.environment
              # [ cmd.command ]
              # cmd.arguments
            )
      ++  "\n"
