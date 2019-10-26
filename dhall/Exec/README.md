# Command Wrapper Exec Dhall Library

Library of utilities for Command Wrapper's `exec` subcommand.  Mostly smart
constructors for building `ExecCommand` builders of type:

```
  λ(verbosity : Verbosity)
→ λ(colourOutput : ColourOutput)
→ λ(arguments : List Text)
→ ExecCommand
```

## Table of Contents

*   [Importing](#importing)
*   [Provided Smart Constructors](#provided-smart-constructors)


## Importing

There are two basic ways of importing this library:

*   Latest version:

    ```Dhall
    let Exec =
          https://raw.githubusercontent.com/trskop/command-wrapper/master/dhall/Exec/package.dhall

    in  Exec
    ```

*   Specific version (notice the commit hash in the URL):

    ```Dhall
    let Exec =
          https://raw.githubusercontent.com/trskop/command-wrapper/d12e686ef9f68ed91a71f66810f86205cd8b4376/dhall/Exec/package.dhall

    in  Exec
    ```

To make sure that import is protected by semantic hash use:

```
TOOLSET config --dhall-freeze
```

For more information see:

```
TOOLSET help [--man] config
```


## Provided Smart Constructors

Smart constructor for following commands/tools is provided:

*   [`bazel`](./bazel) -- Build automation tool. <https://bazel.build/>
*   [`buildifier`](./buildifier) -- Tool for formatting bazel BUILD and .bzl
    files with a standard convention.
    <https://github.com/bazelbuild/buildtools/tree/master/buildifier>
*   [`direnv`](./direnv) is a shell extension that modifies shell environment
    based on current directory. <https://direnv.net/>
*   [`docker`](./docker)
*   [`docker-compose`](./docker-compose) <https://docs.docker.com/compose/>
*   [`firefox`](./firefox)
*   [`go-jira`](./go-jira) -- Simple Jira command line client
    <https://github.com/go-jira/jira>
*   [`jq`](./jq) <https://stedolan.github.io/jq/>
*   [`pg_dump`](./pg_dump)
*   [`psql`](./psql)
*   [`run-mailcap`](./run-mailcap)
*   [`ssh`](./ssh)
*   [`stack`](./stack) <https://docs.haskellstack.org/>
*   [`tmux`](./tmux) <https://tmux.github.io/>
*   [`xdg-open`](./xdg-open)
*   [`yarn`](./yarn) <https://yarnpkg.com/>


## Other Utilities

*   [`completion`](./completion) -- Helper functions and scripts for command
    line completion of Exec commands.

*   [`utils`](./utils):

    *   [`utils/colourOutputOptions`](./utils/colourOutputOptions) -- Convert
        `ColourOutput` value into command line options.  Usage example:

         ```Dhall
           colourOutputOptions
             { Always = [ "-C" ], Auto = [] : List Text, Never = [ "-M" ] }
        : ColourOutput → List Text
         ```

    *   [`utils/optionalOptions`](./utils/optionalOptions) -- Convert an
        `Optional` value into command line options, or empty list if it's
        `None`.

        ```Dhall
          optionalOptions
            Text
            (λ(dir : Text) → [ "--directory=${dir}" ])
            (Some "/a/directory")
        : Optional Text → List Text
        ```

    *   [`utils/to-shell`](./utils/to-shell)  -- Convert output of
        `TOOLSET exec --print COMMAND` into command that can be executed on
        command line.  See [`utils/to-shell`](./utils/to-shell) for examples.

    *   [`utils/verbosityOptions`](./utils/verbosityOptions) -- Convert
        `Verbosity` value into command line options.  Usage example:

        ```Dhall
          verbosityOptions
            { Silent = [ "--quiet" ]
            , Normal = [] : List Text
            , Verbose = [ "--verbose" ]
            , Annoying = [ "--debug" ]
            }
        : Verbosity → List Text
        ```

