# Command Wrapper Dhall Library

Types, defaults and utilities for configuring Command Wrapper toolset and
external subcommands installed with it by default.


## Table of Contents

*   [Importing](#importing)


## Importing

There multiple ways of importing this library:

*   Version from when Command Wrapper binary was compiled:

    ```Bash
    export COMMAND_WRAPPER_LIB="$(
        "${TOOLSET}" completion --library --dhall=command-wrapper --import
    )"
    ```

    ```
    let CommandWrapper = env:COMMAND_WRAPPER_LIB

    in  CommandWrapper
    ```

*   Version embedded in Command Wrapper:

    ```Bash
    "${TOOLSET}" completion --library --dhall=command-wrapper --content > ./CommandWrapper
    ```

    ```Dhall
    lib CommandWrapper = ./CommandWrapper

    in  CommandWrapper
    ```

*   Latest version:

    ```Dhall
    let CommandWrapper =
          https://raw.githubusercontent.com/trskop/command-wrapper/master/command-wrapper/dhall/CommandWrapper/package.dhall

    in  CommandWrapper
    ```

*   Specific version (notice the tag in the URL, commit hash can be used too):

    ```Dhall
    let CommandWrapper =
          https://raw.githubusercontent.com/trskop/command-wrapper/0.1.0.0-rc9/command-wrapper/dhall/CommandWrapper/package.dhall

    in  CommandWrapper
    ```

To make sure that import is protected by semantic hash use:

```
TOOLSET config --dhall-freeze
```

For more information see:

```
TOOLSET help [--man] config
```
