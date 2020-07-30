# jq

Command-line JSON processor <https://stedolan.github.io/jq/>.


## Usage Example

```Dhall
let CommandWrapper =
      https://raw.githubusercontent.com/trskop/command-wrapper/master/command-wrapper/dhall/CommandWrapper/package.dhall

let Exec =
      https://raw.githubusercontent.com/trskop/command-wrapper/master/command-wrapper/dhall/Exec/package.dhall

let toolset = env:COMMAND_WRAPPER_EXE as Text ? "yx"

let jq =
      let -- Command line arguments passed to `jq`.
          arguments =
            CommandWrapper.Command.emptyArguments

      in  CommandWrapper.ExecNamedCommand::{
          , name = "jq"
          , command = Exec.jq.command Exec.jq.Options::{=} arguments
          , completion = Some (Exec.jq.completion toolset (None Text) arguments)
          }

in  [ jq ] : List CommandWrapper.ExecNamedCommand.Type
```
