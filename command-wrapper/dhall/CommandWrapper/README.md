# Command Wrapper Dhall Library

Types, defaults and utilities for configuring Command Wrapper toolset and
external subcommands installed with it by default.


## Table of Contents

*   [Importing](#importing)


## Importing

There are two basic ways of importing this library:

*   Latest version:

    ```Dhall
    let CommandWrapper =
          https://raw.githubusercontent.com/trskop/command-wrapper/master/dhall/CommandWrapper/package.dhall

    in  CommandWrapper
    ```

*   Specific version (notice the commit hash in the URL):

    ```Dhall
    let CommandWrapper =
          https://raw.githubusercontent.com/trskop/command-wrapper/d12e686ef9f68ed91a71f66810f86205cd8b4376/dhall/CommandWrapper/package.dhall

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
