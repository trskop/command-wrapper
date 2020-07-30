-- vim: filetype=dhall
--
-- See <https://developer.mozilla.org/en-US/docs/Mozilla/Command_Line_Options>
-- for more details on Firefox command line options.

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

let Options =
      { Type =
            ./Options/Type.dhall sha256:054af9262f73048f90af26b55ff29e9d6cb0b4b7f8002bdb1994495636f4cc84
          ? ./Options/Type.dhall
      , toArguments =
            ./Options/toArguments.dhall sha256:44011c82c619ad18897156096d65fe26362bc2653c94518300d8453de721b0ec
          ? ./Options/toArguments.dhall
      , toEnvironment =
            ./Options/toEnvironment.dhall sha256:ff5b3e81adc9bd518790ee80f9dc4f0aaac00f2dfe412ab567a79e963e998b7d
          ? ./Options/toEnvironment.dhall
      }

let command =
      λ(options : Options.Type) →
      λ(verbosity : Verbosity) →
      λ(colourOutput : ColourOutput) →
      λ(arguments : List Text) →
        ExecCommand::{
        , command = "firefox"
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
