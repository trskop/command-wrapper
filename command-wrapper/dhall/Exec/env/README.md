# `/usr/bin/env`

Smart constructor that wraps instance of `Verbosity → ColourOutput → List Text
→ ExecCommand` in a `/usr/bin/env` call. In other words if we have command:

```
COMMAND [ARGUMENTS]
```

Then it is turned into:

```
/usr/bin/env [-i] [-u NAME [...]] [NAME=VALUE [...]] COMMAND [ARGUMENTS]
```

This can be useful in cases when we want to:

*   Undefine environment variable or clean environment before `COMMAND` is
    executed.
*   Use different `PATH` to search for `COMMAND` instead of current one.

FreeBSD `env` manual page: [freebsd.org/cgi/man.cgi?env
](https://www.freebsd.org/cgi/man.cgi?env)

GNU `env` manual page: [man7.org/linux/man-pages/man1/env.1.html
](https://man7.org/linux/man-pages/man1/env.1.html)


## Usage Example

```Dhall
let CommandWrapper =
      https://raw.githubusercontent.com/trskop/command-wrapper/master/command-wrapper/dhall/CommandWrapper/package.dhall

let Exec =
      https://raw.githubusercontent.com/trskop/command-wrapper/master/command-wrapper/dhall/Exec/package.dhall

let build-static =
      CommandWrapper.ExecNamedCommand::{
      , name = "build"
      , description = Some "Run our Shake-based build system"
      , command =
          Exec.env.command
            Exec.env.Options::{
            , define =
              [ { name = "PATH"
                , value = "${env:PATH as Text}:/some/path/to/project/bin"
                }
              ]
            }
            ( Exec.shake.command
                "shake-until-built"
                Exec.shake.Options::{=}
                CommandWrapper.Environment.empty
                [ "build-static" ]
            )
      }

in  build-static : CommandWrapper.ExecNamedCommand.Type
```
