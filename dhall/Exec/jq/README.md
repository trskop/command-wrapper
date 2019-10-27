# jq

Command-line JSON processor <https://stedolan.github.io/jq/>.


## Usage Example

```Dhall
let CommandWrapper =
      https://raw.githubusercontent.com/trskop/command-wrapper/master/dhall/CommandWrapper/package.dhall

let Exec =
      https://raw.githubusercontent.com/trskop/command-wrapper/master/dhall/Exec/package.dhall

let toolset = "yx"

let jq =
      let -- Command line arguments passed to `jq`.
          arguments = CommandWrapper.Command.emptyArguments

      in  CommandWrapper.ExecNamedCommand::{
          , name = "jq"
          , command = Exec.jq.command arguments
          , completion =
              Some (Exec.jq.completion toolset (None Text) arguments)
          }

in  [ jq ] : List CommandWrapper.ExecNamedCommand.Type
```
