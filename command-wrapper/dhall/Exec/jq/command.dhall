-- vim: filetype=dhall
--
-- ```
-- jq [OPTIONS] [--colour-output|--monochrome-output] [ARGUMENTS] [EXTRA_ARGUMENTS]
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

let Options =
      { Type =
            ./Options/Type.dhall sha256:888713faabcc80155c73e123229956496a179aeb6737d9c460107ab0b59c373f
          ? ./Options/Type.dhall
      , toArguments =
            ./Options/toArguments.dhall sha256:fa280e851fa4c2ee59d24cddd7b999922026ed7f5ebed28a965b77e9cc3be2e5
          ? ./Options/toArguments.dhall
      , toEnvironment =
            ./Options/toEnvironment.dhall sha256:721b4304bfbffa12d38cde308d54bc96348e9820101f22fef487bac777af95bb
          ? ./Options/toEnvironment.dhall
      }

let OutputColour =
        ./OutputColour/package.dhall sha256:2297c975b893a070f00082bba345a64480cb073f59dc5155424829fadd4765af
      ? ./OutputColour/package.dhall

let -- We don't override user settings, only when 'outputColour' is not set we
    -- try to apply 'colourOutput' value given to us by the toolset.
    amendOptions =
      λ(options : Options.Type) →
      λ(_ : Verbosity) →
      λ(colourOutput : ColourOutput) →
          options
        ⫽ { outputColour =
              merge
                { None = OutputColour.fromColourOutput colourOutput
                , Some = λ(_ : OutputColour.Type) → Some _
                }
                options.outputColour
          }

let command =
      λ(unalteredOptions : Options.Type) →
      λ(extraOptions : List Text) →
      λ(verbosity : Verbosity) →
      λ(colourOutput : ColourOutput) →
      λ(arguments : List Text) →
        let options = amendOptions unalteredOptions verbosity colourOutput

        in  ExecCommand::{
            , command = "jq"
            , arguments = Options.toArguments options # extraOptions # arguments
            , workingDirectory = options.workingDirectory
            , environment = Options.toEnvironment options
            }

in    command
    : Options.Type →
      ∀(extraOptions : List Text) →
      Verbosity →
      ColourOutput →
      ∀(arguments : List Text) →
        ExecCommand.Type
