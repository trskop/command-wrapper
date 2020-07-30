-- vim: filetype=dhall
--
-- ```
-- fzf [OPTIONS] [ARGUMENTS]
-- ```

let ExecCommand =
      { Type =
            ../../CommandWrapper/ExecCommand/Type sha256:6ece797a2c269b469da41f12ec2d7206846cac65183ae8213cce1b6d59f2b02b
          ? ../../CommandWrapper/ExecCommand/Type
      , default =
            ../../CommandWrapper/ExecCommand/default sha256:c3a088ca2b090c91d5d630c4e01f4b4fbd0136f5bb1251f6828698e5180685b2
          ? ../../CommandWrapper/ExecCommand/default
      }

let Verbosity =
        ../../CommandWrapper/Verbosity/Type sha256:f009a39a49b1ee65651e9510778e7d72ff96820f4702a955e8f47682d72995c6
      ? ../../CommandWrapper/Verbosity/Type

let ColourOutput =
        ../../CommandWrapper/ColourOutput/Type sha256:0ef91a3f044406ee80fc20f26127b448a0e98f46c46ec024457023d2aded8543
      ? ../../CommandWrapper/ColourOutput/Type

let Options =
      { Type =
            ./Options/Type.dhall sha256:54460863b32e729cf617282abfc10164e5ac65565bb8b71099b27567e6404a3d
          ? ./Options/Type.dhall
      , toArguments =
            ./Options/toArguments.dhall sha256:67b247e0c720e969929ecacf4495c93deddb63d7b5c219ffa051941f7159ec90
          ? ./Options/toArguments.dhall
      , toEnvironment =
            ./Options/toEnvironment.dhall sha256:7a056d025a1fadf0e2a2ff018a04922b99ffe6d58ef62248479e74a29831b5e3
          ? ./Options/toEnvironment.dhall
      }

let command =
      λ(options : Options.Type) →
      λ(_ : Verbosity) →
      λ(_ : ColourOutput) →
      λ(arguments : List Text) →
        ExecCommand::{
        , command = "fzf"
        , arguments = Options.toArguments options # arguments
        , workingDirectory = options.workingDirectory
        , environment = Options.toEnvironment options
        }

in    command
    : Options.Type →
      Verbosity →
      ColourOutput →
      ∀(arguments : List Text) →
        ExecCommand.Type
