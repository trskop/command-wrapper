-- vim: filetype=dhall
--
-- ```
-- nvr [OPTIONS] [EXTRA_ARGUMENTS]
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
            ./Options/Type.dhall sha256:d10faab8c8388e68d0e931248dee4741cbcef8b593095d8dd8a42a54c2272ab5
          ? ./Options/Type.dhall
      , toEnvironment =
            ./Options/toEnvironment.dhall sha256:660fd2452fe3c69253efb6b72e7f5b19e27dcdd13db8cb456d9d0579d20944ea
          ? ./Options/toEnvironment.dhall
      , toArguments =
            ./Options/toArguments.dhall sha256:dc15673ba58fb5d66d1fe8ed53ed3ec2c124c8cb0ed6be4b75fa6285f2bc841c
          ? ./Options/toArguments.dhall
      }

in  λ(options : Options.Type) →
    λ(_ : Verbosity) →
    λ(_ : ColourOutput) →
    λ(extraArguments : List Text) →
      ExecCommand::{
      , command = "nvr"
      , arguments = Options.toArguments options # extraArguments
      , workingDirectory = options.workingDirectory
      , environment = Options.toEnvironment options
      }
