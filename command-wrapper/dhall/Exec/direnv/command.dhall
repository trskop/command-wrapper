-- vim: filetype=dhall

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

let Environment =
        ../../CommandWrapper/Environment/Type sha256:bfc2cb080bb0cac5a42a81beb707437389fa9c6f8e54ae8dd7ce09c6566140ee
      ? ../../CommandWrapper/Environment/Type

let Options =
        ./Options/package.dhall sha256:f40d104b121ea8568116570f0eebafc8853c7983ef3e9efcd40ad9326a92e481
      ? ./Options/package.dhall

let command =
      λ(options : Options.Type) →
      λ(arguments : List Text) →
      λ(verbosity : Verbosity) →
      λ(colourOutput : ColourOutput) →
      λ(extraArguments : List Text) →
        ExecCommand::{
        , command = "direnv"
        , arguments = Options.toArguments options # arguments # extraArguments
        , workingDirectory = options.workingDirectory
        , environment = Options.toEnvironment options
        }

in    command
    : Options.Type →
      ∀(arguments : List Text) →
      Verbosity →
      ColourOutput →
      ∀(extraArguments : List Text) →
        ExecCommand.Type
