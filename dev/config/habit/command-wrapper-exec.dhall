let CommandWrapper = ../lib/CommandWrapper

let ExecNamedCommand = CommandWrapper.ExecNamedCommand

let Exec =
      https://raw.githubusercontent.com/trskop/command-wrapper/ec8e3de9cca65b0b9762621213fcbae6164a6ea1/command-wrapper/dhall/Exec/package.dhall sha256:f156fac2386640f20df7a0a6dccc5d33e31a7563ef0615ca52cc67218d94ce6c

let Options = ./options.dhall

let Verbosity = CommandWrapper.Verbosity

let ColourOutput = CommandWrapper.ColourOutput

let options = Options::{=}

let hostPath =
        env:DIRENV_HOST_PATH sha256:d99c4daf3a10e7052d4c19df09a9c0d34f13fa4eb854679c3e9a159f81bbb88b as Text
      ? env:DIRENV_HOST_PATH as Text

let withHostPath =
      Exec.env.command Exec.env.Options::{define = [ {name = "PATH", value = hostPath }]}

let shake =
      λ(arguments : List Text) →
        withHostPath
          ( Exec.shake.command
              "${options.projectRoot}/Shakefile.hs"
              Exec.shake.Options::{directory = Some options.projectRoot}
              CommandWrapper.Environment.empty
              arguments
          )

let commands =
        [ ExecNamedCommand::{
          , name = "build"
          , description = Some "Build the project"
          , command = shake [ "build" ]
          }
        , ExecNamedCommand::{
          , name = "build-static"
          , description = Some
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
          , description = Some
              "Build and install the manual pages into ~/.local/share/man"
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
                    withHostPath
                      ( Exec.stack.command
                          stack.options
                          stack.arguments
                      )
              , completion = Some
                  ( λ(shell : CommandWrapper.Shell.Type) →
                    λ(index : Natural) →
                    λ(words : List Text) →
                      withHostPath
                        ( λ(_ : Verbosity.Type) →
                          λ(_ : ColourOutput.Type) →
                          λ(_ : List Text) →
                            Exec.stack.completion
                              stack.options
                              stack.arguments
                              shell
                              index
                              ([ "stack" ] # words)
                        )
                        Verbosity.Type.Silent
                        ColourOutput.Type.Never
                        ([] : List Text)
                  )
              }
        ]
      : List ExecNamedCommand.Type

in  CommandWrapper.ExecConfig::{ commands }
