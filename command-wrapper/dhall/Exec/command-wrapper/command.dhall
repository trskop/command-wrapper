-- vim: filetype=dhall
--
-- ```
-- TOOLSET [--verbosity=VERBOSITY] [--colour=COLOUR_OUTPUT] [--[no-]aliases]
--     [--change-directory=DIRECTORY] -- [SUBCOMMAND [ARGUMENT ...]]
-- ```

let Options =
        ./Options/package.dhall sha256:5c50274c1e8693b4424d10d7668f5775d098810393e0d37354282f761d2483da
      ? ./Options/package.dhall

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

let -- It is possible to override `verbosity : Verbosity` and
    -- `colourOutput : ColourOutput` by specifying them in
    -- `options : Options.Type`.
    amendOptions =
      λ(options : Options.Type) →
      λ(verbosity : Verbosity) →
      λ(colourOutput : ColourOutput) →
          options
        ⫽ { verbosity =
              merge
                { None = Some verbosity, Some = λ(_ : Verbosity) → Some _ }
                options.verbosity
          , colour =
              merge
                { None = Some colourOutput
                , Some = λ(_ : ColourOutput) → Some _
                }
                options.colour
          }

let command =
      λ(toolset : Text) →
      λ(unalteredOptions : Options.Type) →
      λ(verbosity : Verbosity) →
      λ(colourOutput : ColourOutput) →
      λ(arguments : List Text) →
        let options = amendOptions unalteredOptions verbosity colourOutput

        in  ExecCommand::{
            , command = toolset
            , arguments = Options.toArguments options # [ "--" ] # arguments
            , environment = Options.toEnvironment options
            }

let test0 =
        assert
      :   command
            "the-tool"
            Options::{=}
            Verbosity.Normal
            ColourOutput.Auto
            ([] : List Text)
        ≡ ExecCommand::{
          , command = "the-tool"
          , arguments = [ "--verbosity=normal", "--colour=auto", "--" ]
          }

in    command
    : ∀(toolset : Text) →
      ∀(options : Options.Type) →
      Verbosity →
      ColourOutput →
      ∀(arguments : List Text) →
        ExecCommand.Type
