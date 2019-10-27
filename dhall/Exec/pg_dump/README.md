# PostgreSQL Client

Extract a PostgreSQL database into a script file or other archive file
<https://www.postgresql.org/docs/current/app-pgdump.html>.


## Usage Example

Presented example uses vanilla [`Exec.pg_dump.command`](./command), however,
it is possible to wrap it in other calls, for example:

*   [`Exec.docker.exec`](../docker/exec) and run it inside a Docker container.
*   [`Exec.ssh.command`](../ssh/command) and run on a remote machine.
*   Combine both [`Exec.ssh.command`](../ssh/command) and
    [`Exec.docker.exec`](../docker/exec), to run it in a Docker container on a
    remote machine.

```Dhall
let CommandWrapper =
      https://raw.githubusercontent.com/trskop/command-wrapper/master/dhall/CommandWrapper/package.dhall

let Exec =
      https://raw.githubusercontent.com/trskop/command-wrapper/master/dhall/Exec/package.dhall

let toolset = env:COMMAND_WRAPPER_EXE as Text ? "yx"

let emptyEnvironment = CommandWrapper.Command.emptyEnvironment

let pg_dump =
      let -- Database to connect to and under which user:
          connect =
            CommandWrapper.ConnectToDatabase::{
            , hostname = "localhost"
            , database = "development"
            , username = "developer"
            }

      in  CommandWrapper.ExecNamedCommand::{
          , name = "pg_dump.development"
          , description = Some "Dump local development PostgreSQL DB."
          , command = Exec.pg_dump.command connect emptyEnvironment
          }

in  [ pg_dump ] : List CommandWrapper.ExecNamedCommand.Type
```
