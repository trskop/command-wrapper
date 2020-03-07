let CommandWrapper = ../lib/CommandWrapper

let ExecNamedCommand = CommandWrapper.ExecNamedCommand

let ExecCommand = CommandWrapper.ExecCommand

let Options = ./options.dhall

let options = Options::{=}

let shake =
        λ(arguments : List Text)
      → λ(_ : < Annoying | Normal | Silent | Verbose >)
      → λ(colour : < Always | Auto | Never >)
      → λ(extraArguments : List Text)
      → ExecCommand::{
        , command = "env"
        , arguments =
              [ "PATH=${env:DIRENV_HOST_PATH as Text}"
              , "${options.projectRoot}/Shakefile.hs"
              ]
            # merge
                { Always = [ "--colour" ]
                , Auto = [ "--colour" ]
                , Never = [] : List Text
                }
                colour
            # [ "build" ]
            # arguments
            # extraArguments
        , workingDirectory = Some "${options.projectRoot}"
        }

let commands =
      [ ExecNamedCommand::{ name = "build", command = shake [ "build" ] }
      , ExecNamedCommand::{
        , name = "build-static"
        , command = shake [ "static" ]
        }
      , ExecNamedCommand::{ name = "test", command = shake [ "test" ] }
      , ExecNamedCommand::{ name = "install", command = shake ([] : List Text) }
      ]
    : List ExecNamedCommand.Type

in  CommandWrapper.ExecConfig::{ commands = commands }
