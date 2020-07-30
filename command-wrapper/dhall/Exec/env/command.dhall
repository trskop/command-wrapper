-- vim: filetype=dhall
--
-- ```
-- /usr/bin/env [-i] [-u NAME [...]] [NAME=VALUE [...]] COMMAND [ARGUMENTS]
-- ```

let Options =
        ./Options/package.dhall sha256:8ff31a81e6728a1fe6485eebedf31db89afbb738fb9644e9f3ae00e50a58247f
      ? ./Options/package.dhall

let ExecCommand =
        ../../CommandWrapper/ExecCommand/package.dhall sha256:3d1f1db23e794116bfc80efad94a291087792b0a96d8f5be5b1c9276657d0663
      ? ../../CommandWrapper/ExecCommand/package.dhall

let Verbosity =
        ../../CommandWrapper/Verbosity/Type sha256:f009a39a49b1ee65651e9510778e7d72ff96820f4702a955e8f47682d72995c6
      ? ../../CommandWrapper/Verbosity/Type

let ColourOutput =
        ../../CommandWrapper/ColourOutput/Type sha256:0ef91a3f044406ee80fc20f26127b448a0e98f46c46ec024457023d2aded8543
      ? ../../CommandWrapper/ColourOutput/Type

let command =
      λ(options : Options.Type) →
      λ(constructor : Verbosity → ColourOutput → List Text → ExecCommand.Type) →
      λ(verbosity : Verbosity) →
      λ(colour : ColourOutput) →
      λ(arguments : List Text) →
        let command = constructor verbosity colour arguments

        in  ExecCommand::{
            , command = "/usr/bin/env"
            , arguments =
                  Options.toArguments options
                # [ command.command ]
                # command.arguments
            , searchPath = False
            , workingDirectory =
                merge
                  { None = command.workingDirectory
                  , Some = λ(dir : Text) → Some dir
                  }
                  options.workingDirectory
            , environment = Options.toEnvironment options # command.environment
            }

in    command
    : Options.Type →
      (Verbosity → ColourOutput → List Text → ExecCommand.Type) →
      Verbosity →
      ColourOutput →
      ∀(arguments : List Text) →
        ExecCommand.Type
