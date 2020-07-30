# SSH

[OpenSSH](https://www.openssh.com/) client `ssh` allows secure login to remote
hosts.

Homepage: [openssh.com](https://www.openssh.com/)

Manual page: [man.openbsd.org/ssh.1](https://man.openbsd.org/ssh.1)


## Usage Example

```Dhall
let CommandWrapper =
      https://raw.githubusercontent.com/trskop/command-wrapper/master/command-wrapper/dhall/CommandWrapper/package.dhall

let Exec =
      https://raw.githubusercontent.com/trskop/command-wrapper/master/command-wrapper/dhall/Exec/package.dhall

let noExtraArguments = [] : List Text

let ssh =
      let host = "my-remote-machine"

      in  CommandWrapper.ExecNamedCommand::{
          , name = "ssh.${host}"
          , command =
              Exec.ssh.command
                Exec.ssh.Options::{=}
                Exec.ssh.SshTo::{ host }
                noExtraArguments
          }

in  ssh : CommandWrapper.ExecNamedCommand.Type
```
