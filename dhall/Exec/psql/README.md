# PostgreSQL Client

PostgreSQL interactive terminal `psql`
<https://www.postgresql.org/docs/current/app-psql.html>.


## Usage Example

```Dhall
let CommandWrapper =
      https://raw.githubusercontent.com/trskop/command-wrapper/master/dhall/CommandWrapper/package.dhall

let Exec =
      https://raw.githubusercontent.com/trskop/command-wrapper/master/dhall/Exec/package.dhall

let toolset = env:COMMAND_WRAPPER_EXE as Text ? "yx"

let emptyEnvironment = CommandWrapper.Command.emptyEnvironment

let psql =
      let -- Custom PGPASSFILE:
          pgpassFile =
            None Text

      let -- Custom .psqlrc file:
          psqlrcFile =
            None Text

      let -- Database to connect to and under which user:
          connect =
            CommandWrapper.ConnectToDatabase::{
            , hostname = "localhost"
            , database = "development"
            , username = "developer"
            }

      in  CommandWrapper.ExecNamedCommand::{
          , name = "psql.development"
          , description = Some "Connect to local development PostgreSQL DB."
          , command =
              Exec.psql.command pgpassFile psqlrcFile connect emptyEnvironment
          }

in  [ psql ] : List CommandWrapper.ExecNamedCommand.Type
```
