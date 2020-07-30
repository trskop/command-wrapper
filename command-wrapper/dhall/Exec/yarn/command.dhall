-- vim: filetype=dhall
--
-- ```
-- yarn [--silent|--verbose] [--cwd=DIRECTORY] [PREFIX_ARGUMENTS] [ARGUMENTS]
-- ```
--
-- TODO: Handle `ColourOutput`:
--
-- * <https://stackoverflow.com/questions/49908253/yarn-how-to-globally-disable-color-output>
-- * <https://github.com/chalk/chalk#chalksupportscolor>

let ExecCommand =
      { Type =
            ../../CommandWrapper/ExecCommand/Type sha256:6ece797a2c269b469da41f12ec2d7206846cac65183ae8213cce1b6d59f2b02b
          ? ../../CommandWrapper/ExecCommand/Type
      , default =
            ../../CommandWrapper/ExecCommand/default sha256:c3a088ca2b090c91d5d630c4e01f4b4fbd0136f5bb1251f6828698e5180685b2
          ? ../../CommandWrapper/ExecCommand/default
      }

let Verbosity =
      { Type =
            ../../CommandWrapper/Verbosity/Type sha256:f009a39a49b1ee65651e9510778e7d72ff96820f4702a955e8f47682d72995c6
          ? ../../CommandWrapper/Verbosity/Type
      , fold =
            ../../CommandWrapper/Verbosity/fold sha256:4dac2c264a2531d569ad0e5f712a1cd2d17b51ecdc502cc72f19937bf4733b1e
          ? ../../CommandWrapper/Verbosity/fold
      }

let ColourOutput =
        ../../CommandWrapper/ColourOutput/Type sha256:0ef91a3f044406ee80fc20f26127b448a0e98f46c46ec024457023d2aded8543
      ? ../../CommandWrapper/ColourOutput/Type

let Options =
        ./Options/package.dhall sha256:8965af774ce9c8806adc28e08e94b513a223cba3d5361808ec6f4f978d3c8774
      ? ./Options/package.dhall

let amendOptions =
      λ(options : Options.Type) →
      λ(verbosity : Verbosity.Type) →
      λ(colourOutput : ColourOutput) →
          options
        ⫽ { verbosity =
              let YarnVerbosity = < silent | verbose >
              
              in  merge
                    { None =
                        Verbosity.fold
                          (Optional YarnVerbosity)
                          { Silent = Some YarnVerbosity.silent
                          , Normal = None YarnVerbosity
                          , Verbose = Some YarnVerbosity.verbose
                          , Annoying = Some YarnVerbosity.verbose
                          }
                          verbosity
                    , Some = λ(_ : YarnVerbosity) → Some _
                    }
                    options.verbosity
          }

let command =
      λ(unalteredOptions : Options.Type) →
      λ(extraOptions : List Text) →
      λ(verbosity : Verbosity.Type) →
      λ(colourOutput : ColourOutput) →
      λ(arguments : List Text) →
        let options = amendOptions unalteredOptions verbosity colourOutput
        
        in  ExecCommand::{
            , command = "yarn"
            , arguments = Options.toArguments options # extraOptions # arguments
            , environment = Options.toEnvironment options
            }

let noArguments = [] : List Text

let test0 =
        assert
      :   command
            Options::{=}
            noArguments
            Verbosity.Type.Normal
            ColourOutput.Auto
            noArguments
        ≡ ExecCommand::{ command = "yarn" }

let test1 =
        assert
      :   command
            Options::{ workingDirectory = Some "/path/to/a/directory" }
            [ "build" ]
            Verbosity.Type.Normal
            ColourOutput.Auto
            noArguments
        ≡ ExecCommand::{
          , command = "yarn"
          , arguments = [ "--cwd=/path/to/a/directory", "build" ]
          }

in    command
    : Options.Type →
      ∀(extraOptions : List Text) →
      Verbosity.Type →
      ColourOutput →
      ∀(arguments : List Text) →
        ExecCommand.Type
