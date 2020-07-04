# Fzf

Fzf (`fzf`) is a general-purpose command-line fuzzy finder.
[github.com/junegunn/fzf](https://github.com/junegunn/fzf)


## Usage Example

```Dhall
let CommandWrapper =
      https://raw.githubusercontent.com/trskop/command-wrapper/master/command-wrapper/dhall/CommandWrapper/package.dhall

let Exec =
      https://raw.githubusercontent.com/trskop/command-wrapper/master/command-wrapper/dhall/Exec/package.dhall

let fzf =
      let workDirectory = Some "/my/work/dir"

      in  CommandWrapper.ExecNamedCommand::{
          , name = "fzf"
          , description = Some "Run 'fzf'"
          , command =
              Exec.fzf.command
                workDirectory
                Exec.fzf.Options::{
                , layout =
                    Some
                      < BottomOfTheScreen
                      | TopOfTheScreen
                      | TopOfTheScreenPromptAtTheBottom
                      >.TopOfTheScreen
                , height =
                    Some
                      (< Lines : Natural | Percentage : Natural >.Percentage 40)
                }
          }

in  fzf
```
