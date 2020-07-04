# Bazel

Bazel is a build automation tool.

Homepage: [bazel.build](https://bazel.build/)

GitHub organisation: [github.com/bazelbuild](https://github.com/bazelbuild)


## Usage Example

```Dhall
let CommandWrapper =
      https://raw.githubusercontent.com/trskop/command-wrapper/master/command-wrapper/dhall/CommandWrapper/package.dhall

let Exec =
      https://raw.githubusercontent.com/trskop/command-wrapper/master/command-wrapper/dhall/Exec/package.dhall

let toolset = env:COMMAND_WRAPPER_EXE as Text ? "yx"

let bazel-build-work =
      let workingDirectory = Some "/my/work/repo"

      let arguments = [ "build" ]

      in  CommandWrapper.ExecNamedCommand::{
          , name = "work.build"
          , command = Exec.bazel.command workingDirectory arguments
          , completion = Some
              (Exec.bazel.completion toolset workingDirectory arguments)
          }

in  [ bazel-build-work ] : List CommandWrapper.ExecNamedCommand.Type
```
