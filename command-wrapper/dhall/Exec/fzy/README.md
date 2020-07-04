# Fzy

Fzy (`fzy`) is a fast, simple fuzzy text selector for the terminal with an
advanced scoring algorithm.  <https://github.com/jhawthorn/fzy>


## Usage Example

```Dhall
let CommandWrapper =
      https://raw.githubusercontent.com/trskop/command-wrapper/master/command-wrapper/dhall/CommandWrapper/package.dhall

let Exec =
      https://raw.githubusercontent.com/trskop/command-wrapper/master/command-wrapper/dhall/Exec/package.dhall

let fzy-in-direnv =
      let workDirectory = Some "/my/work/dir"

      in  CommandWrapper.ExecNamedCommand::{
          , name = "fzy"
          , description = Some "Run 'fzy' inside direnv."
          , command =
              Exec.direnv.exec
                workDirectory
                (Exec.fzy.command Exec.fzy.Options::{=})
                CommandWrapper.Environment.empty
          }

in  [ fzy-in-direnv ] : List CommandWrapper.ExecNamedCommand.Type
```
