# Direnv

Direnv is a shell extension that modifies shell environment based on current
directory <https://direnv.net/>.


## Usage Example

```Dhall
let CommandWrapper =
      https://raw.githubusercontent.com/trskop/command-wrapper/master/dhall/CommandWrapper/package.dhall

let Exec =
      https://raw.githubusercontent.com/trskop/command-wrapper/master/dhall/Exec/package.dhall

let toolset = env:COMMAND_WRAPPER_EXE as Text ? "yx"

let bazel-in-direnv =
      let arguments = CommandWrapper.Command.emptyArguments

      let environment = CommandWrapper.Command.emptyEnvironment

      let direnvDirectory = Some "/my/work/repo"

      let workingDirectory = direnvDirectory

      in  CommandWrapper.ExecNamedCommand::{
          , name = "run.bazel"
          , description = Some "Run Bazel inside direnv."
          , command =
              Exec.direnv.exec
                direnvDirectory
                (Exec.bazel.command workingDirectory arguments)
                environment
          , completion =
              Some (Exec.bazel.completion toolset workingDirectory arguments)
          }

in  [ bazel-in-direnv ] : List CommandWrapper.ExecNamedCommand.Type
```
