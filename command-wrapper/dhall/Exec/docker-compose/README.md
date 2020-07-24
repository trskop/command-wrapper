# docker-compose

Docker Compose is a tool for defining and running multi-container Docker
applications.

[docs.docker.com/compose](https://docs.docker.com/compose/)


## Usage Example

```Dhall
let CommandWrapper =
      https://raw.githubusercontent.com/trskop/command-wrapper/master/command-wrapper/dhall/CommandWrapper/package.dhall

let Exec =
      https://raw.githubusercontent.com/trskop/command-wrapper/master/command-wrapper/dhall/Exec/package.dhall

let toolset = env:COMMAND_WRAPPER_EXE as Text ? "yx"

let ColourOutput = CommandWrapper.ColourOutput.Type

in  λ(project : Text) →
      let Action = Exec.docker-compose.Action

      let docker-compose =
            let globalOptions =
                  Exec.docker-compose.GlobalOptions::{
                  , files =
                    [ "${project}/infrastructure/proxies/docker-compose.yaml"
                    , "${project}/infrastructure/services/docker-compose.yaml"
                    ]
                  , workingDirectory = Some project
                  }

            in    Exec.docker-compose
                ⫽ { command = Exec.docker-compose.command globalOptions
                  , completion =
                      λ(action : Optional (ColourOutput → Action.Type)) →
                        Some
                          ( Exec.docker-compose.completion
                              toolset
                              (Some project)
                              ( merge
                                  { None = [] : List Text
                                  , Some =
                                      λ(_ : ColourOutput → Action.Type) →
                                        Exec.docker-compose.Action.toArguments
                                          (_ ColourOutput.Auto)
                                  }
                                  action
                              )
                          )
                  }

      in  [ let up =
                  Action.up
                    ( λ(colourOutput : ColourOutput) →
                        docker-compose.UpOptions::{
                        , noColour =
                            merge
                              { Always = False, Auto = False, Never = True }
                              colourOutput
                        , buildImages = Some True
                        , detach = True
                        }
                    )

            in  CommandWrapper.ExecNamedCommand::{
                , name = "infrastructure.up"
                , description = Some "Start infrastructure"
                , command = docker-compose.command up
                , completion = docker-compose.completion up
                }
          , let down = Action.down docker-compose.DownOptions::{=}

            in  CommandWrapper.ExecNamedCommand::{
                , name = "infrastructure.down"
                , description = Some "Stop infrastructure"
                , command = docker-compose.command down
                , completion = docker-compose.completion down
                }
          ]
```
