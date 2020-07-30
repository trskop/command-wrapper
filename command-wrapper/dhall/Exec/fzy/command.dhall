-- vim: filetype=dhall
--
-- ```
-- fzy [OPTIONS] [EXTRA_ARGUMENTS]
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
            ./Options/Type.dhall sha256:e908cbddb6ec0b95462e513b70eaf6181aed7908212ad1f5614ee736bc174c9c
          ? ./Options/Type.dhall
      , toArguments =
            ./Options/toArguments.dhall sha256:2cd11f648fce2576f14f88a4026f87af8a50f20b274b9489380234a935f9d6ba
          ? ./Options/toArguments.dhall
      , toEnvironment =
            ./Options/toEnvironment.dhall sha256:b091c450f2f169a972dee5692f839c7bd34b69979d9c989663d195a7e0dd85fb
          ? ./Options/toEnvironment.dhall
      }

let command =
      λ(options : Options.Type) →
      λ(_ : Verbosity) →
      λ(_ : ColourOutput) →
      λ(arguments : List Text) →
        ExecCommand::{
        , command = "fzy"
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
