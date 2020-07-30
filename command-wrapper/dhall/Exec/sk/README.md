# Skim

Fully-featured alternative to [`fzf`](https://github.com/junegunn/fzf) written
in Rust. [github.com/lotabout/skim](https://github.com/lotabout/skim)


## Usage Example

```Dhall
let CommandWrapper =
      https://raw.githubusercontent.com/trskop/command-wrapper/master/command-wrapper/dhall/CommandWrapper/package.dhall

let Exec =
      https://raw.githubusercontent.com/trskop/command-wrapper/master/command-wrapper/dhall/Exec/package.dhall

let skim =
      CommandWrapper.ExecNamedCommand::{
      , name = "sk"
      , description = Some "Run Skim ('sk')"
      , command =
          Exec.sk.command
            Exec.sk.Options::{
            , layout = Some
                < BottomOfTheScreen
                | TopOfTheScreen
                | TopOfTheScreenPromptAtTheBottom
                >.TopOfTheScreen
            , height = Some
                (< Lines : Natural | Percentage : Natural >.Percentage 40)
            , workDirectory = Some "/my/work/dir"
            }
      }

in  skim : CommandWrapper.ExecNamedCommand.Type
```
