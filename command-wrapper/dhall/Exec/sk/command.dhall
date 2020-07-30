-- vim: filetype=dhall
--
-- ```
-- sk [OPTIONS] [EXTRA_ARGUMENTS]
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
        ./Options/package.dhall sha256:c9af3f4c7cf78a2c561aa04dd378187556591b5e04cf9c9e44cb11d7e823670b
      ? ./Options/package.dhall

let command =
      λ(options : Options.Type) →
      λ(_ : Verbosity) →
      λ(_ : ColourOutput) →
      λ(arguments : List Text) →
        ExecCommand::{
        , command = "sk"
        , arguments = Options.toArguments options # arguments
        , workingDirectory = options.workingDirectory
        , environment = Options.toEnvironment options
        }

let test0 =
        assert
      :   command
            Options::{=}
            Verbosity.Silent
            ColourOutput.Never
            ([] : List Text)
        ≡ ExecCommand::{ command = "sk" }

let test1 =
        assert
      :   command
            Options::{ workingDirectory = Some "/path/to/a/directory" }
            Verbosity.Silent
            ColourOutput.Never
            [ "--version" ]
        ≡ ExecCommand::{
          , command = "sk"
          , arguments = [ "--version" ]
          , workingDirectory = Some "/path/to/a/directory"
          }

in    command
    : Options.Type →
      Verbosity →
      ColourOutput →
      ∀(arguments : List Text) →
        ExecCommand.Type
