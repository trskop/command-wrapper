# Bazel

Build automation tool <https://bazel.build/>.


## Usage Example

```Dhall
let CommandWrapper =
      https://raw.githubusercontent.com/trskop/command-wrapper/master/dhall/CommandWrapper/package.dhall

let Exec =
      https://raw.githubusercontent.com/trskop/command-wrapper/master/dhall/Exec/package.dhall

let toolset = "yx"

let bazel-build-work =
      let workingDirectory = Some "/my/work/repo"

      let arguments = [ "build" ]

      in  CommandWrapper.ExecNamedCommand::{
          , name = "work.build"
          , command = Exec.bazel.command workingDirectory arguments
          , completion =
              Some (Exec.bazel.completion toolset workingDirectory arguments)
          }

in  [ bazel-build-work ] : List CommandWrapper.ExecNamedCommand.Type
```
