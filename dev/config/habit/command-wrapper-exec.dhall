let CommandWrapper = ../lib/CommandWrapper

let ExecNamedCommand = CommandWrapper.ExecNamedCommand

let ExecCommand = CommandWrapper.ExecCommand

let Exec = ../lib/Exec

let Options = ./options.dhall

let Verbosity = CommandWrapper.Verbosity

let ColourOutput = CommandWrapper.ColourOutput

let options = Options::{=}

let hostPath = env:DIRENV_HOST_PATH as Text

let withHostPath =
        λ(cmd : ExecCommand.Type)
      → ExecCommand::{
        , command = "env"
        , arguments = [ "PATH=${hostPath}", cmd.command ] # cmd.arguments
        , workingDirectory = Some options.projectRoot
        }

let shake =
        λ(arguments : List Text)
      → λ(_ : Verbosity.Type)
      → λ(colour : ColourOutput.Type)
      → λ(extraArguments : List Text)
      → withHostPath
          ExecCommand::{
          , command = "${options.projectRoot}/Shakefile.hs"
          , arguments =
                Exec.utils.colourOutputOptions
                  { Always = [ "--colour" ]
                  , Auto = [ "--colour" ]
                  , Never = [] : List Text
                  }
                  colour
              # arguments
              # extraArguments
          }

let commands =
        [ ExecNamedCommand::{
          , name = "build"
          , description = Some "Build the project"
          , command = shake [ "build" ]
          }
        , ExecNamedCommand::{
          , name = "build-static"
          , description =
              Some
              "Build static binaries inside Docker and package them for release"
          , command = shake [ "static" ]
          }
        , ExecNamedCommand::{
          , name = "test"
          , description = Some "Run tests for the project"
          , command = shake [ "test" ]
          }
        , ExecNamedCommand::{
          , name = "install"
          , description = Some "Build and install into ~/.local"
          , command = shake ([] : List Text)
          }
        , ExecNamedCommand::{
          , name = "install-man"
          , description =
              Some "Build and install the manual pages into ~/.local/share/man"
          , command = shake [ "man" ]
          }
        , let stack =
                { options = Exec.stack.Options::{=}
                , arguments = [] : List Text
                }

          in  ExecNamedCommand::{
              , name = "stack"
              , description = Some "Invoke stack with project-specific settings"
              , command =
                    λ(verbosity : Verbosity.Type)
                  → λ(colour : ColourOutput.Type)
                  → λ(arguments : List Text)
                  → withHostPath
                      ( Exec.stack.command
                          stack.options
                          stack.arguments
                          verbosity
                          colour
                          arguments
                      )
              , completion = Some
                  (   λ(shell : CommandWrapper.Shell.Type)
                    → λ(index : Natural)
                    → λ(words : List Text)
                    → withHostPath
                        ( Exec.stack.completion
                            stack.options
                            stack.arguments
                            shell
                            index
                            ([ "stack" ] # words)
                        )
                  )
              }
        ]
      : List ExecNamedCommand.Type

in  CommandWrapper.ExecConfig::{ commands = commands }
