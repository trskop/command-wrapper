-- vim: filetype=dhall
--
-- Smart constructor for the most generic form of calling a `docker` command:
--
-- ```
-- docker [GLOBAL_OPTIONS] [COMMAND [COMMAND_OPTIONS]] [ARGUMENTS]
-- ```
--
-- See <https://docs.docker.com/engine/reference/commandline/cli/> for more
-- information.

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
        ./Options/package.dhall sha256:b801edf6d8ea64eaad3d358bd788a378a58bd20a167cb8bde32d4fd51b9d1b72
      ? ./Options/package.dhall

let amendOptions =
      λ(options : Options.Type) →
      λ(verbosity : Verbosity) →
      λ(_ : ColourOutput) →
        merge
          { Silent = options
          , Normal = options
          , Verbose = options
          , Annoying = options ⫽ { debug = True }
          }
          verbosity

let command =
      λ(unalteredOptions : Options.Type) →
      λ(commandAndOptions : List Text) →
      λ(verbosity : Verbosity) →
      λ(colourOutput : ColourOutput) →
      λ(arguments : List Text) →
        let options = amendOptions unalteredOptions verbosity colourOutput

        in  ExecCommand::{
            , command = "docker"
            , arguments =
                Options.toArguments options # commandAndOptions # arguments
            , workingDirectory = options.workingDirectory
            , environment = Options.toEnvironment options
            }

let test0 =
        assert
      : let noArguments = [] : List Text

        in    command
                Options::{=}
                noArguments
                Verbosity.Normal
                ColourOutput.Auto
                noArguments
            ≡ ExecCommand::{ command = "docker" }

let test1 =
        assert
      :   command
            Options::{ config = Some "/home/user/.config/docker" }
            [ "system", "prune" ]
            Verbosity.Normal
            ColourOutput.Auto
            [ "--volumes" ]
        ≡ ExecCommand::{
          , command = "docker"
          , arguments =
            [ "--config"
            , "/home/user/.config/docker"
            , "system"
            , "prune"
            , "--volumes"
            ]
          }

in    command
    : Options.Type →
      ∀(commandAndOptions : List Text) →
      Verbosity →
      ColourOutput →
      ∀(arguments : List Text) →
        ExecCommand.Type
