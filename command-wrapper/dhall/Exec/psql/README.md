# PostgreSQL Client

PostgreSQL interactive terminal `psql`
<https://www.postgresql.org/docs/current/app-psql.html>.


## Usage Example

```Dhall
let CommandWrapper =
      https://raw.githubusercontent.com/trskop/command-wrapper/master/command-wrapper/dhall/CommandWrapper/package.dhall

let Exec =
      https://raw.githubusercontent.com/trskop/command-wrapper/master/command-wrapper/dhall/Exec/package.dhall

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

      let extraOptions = [] : List Text

      in  CommandWrapper.ExecNamedCommand::{
          , name = "psql.development"
          , description = Some "Connect to local development PostgreSQL DB."
          , command =
              Exec.psql.command
                Exec.psql.Options::{ pgpassFile, psqlrcFile }
                (Some connect)
                extraOptions
          }

in  [ psql ] : List CommandWrapper.ExecNamedCommand.Type
```
