-- vim: filetype=dhall
--
-- ```
-- youtube-dl [--quiet|--verbose [--print-trafic]] [--no-color] [ARGUMENTS]
--   [EXTRA_ARGUMENTS]
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

let Verbosity/toArguments =
      (   ../utils/verbosityOptions.dhall sha256:53abdd9ed8f27c0d175efc6b33e0a72d1d77661554e3e79e2a23d2c1252aa9a9
        ? ../utils/verbosityOptions.dhall
      )
        { Silent = [ "--quiet" ]
        , Normal = [] : List Text
        , Verbose = [ "--verbose" ]
        , Annoying = [ "--verbise", "--print-traffic" ] : List Text
        }

let ColourOutput/toArguments =
      (   ../utils/colourOutputOptions.dhall sha256:33260b6eaaf2a75fa0261a5e4b6dd3344406907f29fe2ff19838d55d1d18e80c
        ? ../utils/colourOutputOptions.dhall
      )
        { Always = [] : List Text
        , Auto = [] : List Text
        , Never = [ "--no-color" ]
        }

in  λ(workingDirectory : Optional Text) →
    λ(arguments : List Text) →
    λ(verbosity : Verbosity) →
    λ(colourOutput : ColourOutput) →
    λ(extraArguments : List Text) →
      ExecCommand::{
      , command = "youtube-dl"
      , arguments =
            ColourOutput/toArguments colourOutput
          # Verbosity/toArguments verbosity
          # arguments
          # extraArguments
      , workingDirectory
      }
