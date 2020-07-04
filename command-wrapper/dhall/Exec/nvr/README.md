# nvr (neovim-remote)

Neovim-remote allows:

*   Controlling Neovim processes from the shell.  E.g. opening files in another
    terminal window.
*   Opening files from within `:terminal` without starting a nested Neovim
    process.

[github.com/mhinz/neovim-remote](https://github.com/mhinz/neovim-remote)


## Usage Example

```Dhall
let CommandWrapper =
      https://raw.githubusercontent.com/trskop/command-wrapper/master/command-wrapper/dhall/CommandWrapper/package.dhall

let Exec =
      https://raw.githubusercontent.com/trskop/command-wrapper/master/command-wrapper/dhall/Exec/package.dhall

let nvr =
      CommandWrapper.ExecNamedCommand::{
      , name = "nvr"
      , description = Some "Run 'nvr' (neovim-remote)"
      , command =
          Exec.nvr.command
            Exec.nvr.Options::{
            , silent = True
            , serverName = Some "${env:XDG_RUNTIME_DIR as Text}/neovim.socket"
            , action = Some
                ( Exec.nvr.Action.Type.Split
                    { vertical = True, files = [] : List Text }
                )
            }
      }

in  nvr : CommandWrapper.ExecNamedCommand.Type
```
