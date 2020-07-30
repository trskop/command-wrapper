# Direnv

Direnv is a shell extension that modifies shell environment based on current
directory.

Direnv homepage: [direnv.net](https://direnv.net/)

GitHub repository: [github.com/direnv/direnv](https://github.com/direnv/direnv)


## Usage Example

```Dhall
let CommandWrapper =
      https://raw.githubusercontent.com/trskop/command-wrapper/master/command-wrapper/dhall/CommandWrapper/package.dhall

let Exec =
      https://raw.githubusercontent.com/trskop/command-wrapper/master/command-wrapper/dhall/Exec/package.dhall

let toolset = env:COMMAND_WRAPPER_EXE as Text ? "yx"

let bazel-in-direnv =
      let arguments = CommandWrapper.Command.emptyArguments

      let direnvDirectory = Some "/my/work/repo"

      let workingDirectory = direnvDirectory

      in  CommandWrapper.ExecNamedCommand::{
          , name = "run.bazel"
          , description = Some "Run Bazel inside direnv."
          , command =
              Exec.direnv.exec
                Exec.direnv.Options::{ workingDirectory = direnvDirectory }
                (Exec.bazel.command workingDirectory arguments)
          , completion = Some
              (Exec.bazel.completion toolset workingDirectory arguments)
          }

in  [ bazel-in-direnv ] : List CommandWrapper.ExecNamedCommand.Type
```
