-- vim: filetype=dhall
--
-- ```
-- docker-compose [OPTIONS] [ACTION [ACTION_OPTIONS]] [ARGUMENTS]
-- ```

let ExecCommand =
      { Type =
            ../../CommandWrapper/ExecCommand/Type sha256:6ece797a2c269b469da41f12ec2d7206846cac65183ae8213cce1b6d59f2b02b
          ? ../../CommandWrapper/ExecCommand/Type
      , default =
            ../../CommandWrapper/ExecCommand/default sha256:c3a088ca2b090c91d5d630c4e01f4b4fbd0136f5bb1251f6828698e5180685b2
          ? ../../CommandWrapper/ExecCommand/default
      }

let ColourOutput =
        ../../CommandWrapper/ColourOutput/Type sha256:0ef91a3f044406ee80fc20f26127b448a0e98f46c46ec024457023d2aded8543
      ? ../../CommandWrapper/ColourOutput/Type

let Verbosity =
        ../../CommandWrapper/Verbosity/Type sha256:f009a39a49b1ee65651e9510778e7d72ff96820f4702a955e8f47682d72995c6
      ? ../../CommandWrapper/Verbosity/Type

let Action =
        ./Action/package.dhall sha256:c5cd737d9d99aff2dcb3482f1e9082b4db141d4cd04ca5e81f74fe1f4f7a0627
      ? ./Action/package.dhall

let Options =
        ./Options/package.dhall sha256:2c80a359ddb8d2b917793630aeb3371d8d2de96f7b5114bba82d4f0170163c4e
      ? ./Options/package.dhall

let emptyArguments = [] : List Text

let command =
      λ(options : Options.Type) →
      λ(action : Optional (ColourOutput → Action.Type)) →
      λ(_ : Verbosity) →
      λ(colourOutput : ColourOutput) →
      λ(arguments : List Text) →
        ExecCommand::{
        , command = "docker-compose"
        , arguments =
              Options.toArguments options
            # merge
                { None = emptyArguments
                , Some =
                    λ(mkAction : ColourOutput → Action.Type) →
                      Action.toArguments (mkAction colourOutput)
                }
                action
            # arguments
        , environment = Options.toEnvironment options
        , workingDirectory = options.workingDirectory
        }

let test0 =
        assert
      :   command
            Options::{=}
            Action.none
            Verbosity.Normal
            ColourOutput.Auto
            emptyArguments
        ≡ ExecCommand::{ command = "docker-compose" }

in    command
    : Options.Type →
      Optional (ColourOutput → Action.Type) →
      Verbosity →
      ColourOutput →
      ∀(arguments : List Text) →
        ExecCommand.Type
