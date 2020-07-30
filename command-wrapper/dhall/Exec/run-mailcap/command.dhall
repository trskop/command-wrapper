-- vim: filetype=dhall
--
-- ```
-- run-mailcap [OPTIONS] --action={view|cat|compose|composetyped|edit|print} [ARGUMENTS]
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
            ./Options/Type.dhall sha256:674f9d48d01ccb653cd071dd30af0766337d30d0c8fa11498ab3a6e91219479a
          ? ./Options/Type.dhall
      , toArguments =
            ./Options/toArguments.dhall sha256:efdeb9c570b1b569da23b2abca3f0fc1eac3d7dbf14bb4afeda65afc96532af7
          ? ./Options/toArguments.dhall
      , toEnvironment =
            ./Options/toEnvironment.dhall sha256:94f31edf777aa030686c2188d8836ba1c0814e5fa9c560feca369cb9602d333a
          ? ./Options/toEnvironment.dhall
      }

let Action =
      { Type =
            ./Action/Type.dhall sha256:ebe44424cf93ffc0e67bbc651e5fea563e1f88a5b9a2bac59589adb0228b193d
          ? ./Action/Type.dhall
      , toArguments =
            ./Action/toArguments.dhall sha256:94a66162f51c2e7f7ab7143e5d130dd789a04e7cd31dbefbfddaab60db852666
          ? ./Action/toArguments.dhall
      }

let command =
      λ(options : Options.Type) →
      λ(action : Action.Type) →
      λ(extraOptions : List Text) →
      λ(verbosity : Verbosity) →
      λ(colourOutput : ColourOutput) →
      λ(arguments : List Text) →
        ExecCommand::{
        , command = "run-mailcap"
        , arguments =
              Options.toArguments options (Some verbosity) (Some colourOutput)
            # Action.toArguments action
            # extraOptions
            # arguments
        , workingDirectory = options.workingDirectory
        , environment = Options.toEnvironment options
        }

in    command
    : Options.Type →
      Action.Type →
      ∀(extraOptions : List Text) →
      Verbosity →
      ColourOutput →
      ∀(arguments : List Text) →
        ExecCommand.Type
