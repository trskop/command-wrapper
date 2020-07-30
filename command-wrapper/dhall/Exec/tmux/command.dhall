-- vim: filetype=dhall
--
-- ```
-- [TERM=TERMINAL] tmux [-f CONFIG] [ARGUMENTS]
-- ```
--
-- TODO: Handle verbosity and pass `-v` option appropriate number of times.

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
        ./Options/package.dhall sha256:261163963897b080dd24ee84b86bd79f3f7fb36c3a7d3d856d2bc9f7118b5425
      ? ./Options/package.dhall

let command =
      λ(options : Options.Type) →
      λ(extraOptions : List Text) →
      λ(verbosity : Verbosity) →
      λ(colourOutput : ColourOutput) →
      λ(arguments : List Text) →
        ExecCommand::{
        , command = "tmux"
        , arguments = Options.toArguments options # extraOptions # arguments
        , environment = Options.toEnvironment options
        , workingDirectory = options.workingDirectory
        }

let test0 =
        assert
      :   command
            Options::{=}
            ([] : List Text)
            Verbosity.Normal
            ColourOutput.Auto
            ([] : List Text)
        ≡ ExecCommand::{ command = "tmux", arguments = [] : List Text }

let test1 =
        assert
      :   command
            Options::{
            , term = Some "xterm-256color"
            , config = Some "/home/user/.config/tmux/tmux.conf"
            }
            ([] : List Text)
            Verbosity.Normal
            ColourOutput.Auto
            ([] : List Text)
        ≡ ExecCommand::{
          , command = "tmux"
          , arguments = [ "-f", "/home/user/.config/tmux/tmux.conf" ]
          , environment = [ { name = "TERM", value = "xterm-256color" } ]
          }

in    command
    : Options.Type →
      ∀(extraOptions : List Text) →
      Verbosity →
      ColourOutput →
      ∀(arguments : List Text) →
        ExecCommand.Type
