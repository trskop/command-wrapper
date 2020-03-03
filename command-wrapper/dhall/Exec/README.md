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
*   [Command Line Completion Helpers](#command-line-completion-helpers)
*   [Other Utilities](#other-utilities)


## Importing

There multiple ways of importing this library:

*   Version from when Command Wrapper binary was compiled:

    ```Bash
    export COMMAND_WRAPPER_EXEC_LIB="$(
        "${TOOLSET}" completion --library --dhall=exec --import
    )"
    ```

    ```
    let Exec = env:COMMAND_WRAPPER_EXEC_LIB

    in  Exec
    ```

*   Version embedded in Command Wrapper:

    ```Bash
    "${TOOLSET}" completion --library --dhall=exec --content > ./Exec
    ```

    ```Dhall
    lib Exec = ./Exec

    in  Exec
    ```

*   Latest version:

    ```Dhall
    let Exec =
          https://raw.githubusercontent.com/trskop/command-wrapper/master/command-wrapper/dhall/Exec/package.dhall

    in  Exec
    ```

*   Specific version (notice the commit hash in the URL):

    ```Dhall
    let Exec =
          https://raw.githubusercontent.com/trskop/command-wrapper/f85a043ca22407994b0a9183cb43025226463816/command-wrapper/dhall/Exec/package.dhall

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

*   [`bazel`](./bazel) – Build automation tool.
*   [`buildifier`](./buildifier) – Tool for formatting bazel BUILD and `.bzl`
    files with a standard convention.
*   [`command-wrapper`](./command-wrapper) – Smart constructor for calling
    Command Wrapper itself.
*   [`direnv`](./direnv) is a shell extension that modifies shell environment
    based on current directory.
*   [`docker`](./docker)
*   [`docker-compose`](./docker-compose) – Tool for defining and running
    multi-container Docker applications.
*   [`firefox`](./firefox)
*   [`fzf`](./fzf) – Fuzzy text selector.
*   [`fzy`](./fzy) – Fuzzy text selector.
*   [`go-jira`](./go-jira) – Simple Jira command line client.
*   [`jq`](./jq) – Command-line JSON processor.
*   [`nix`](./nix) – Cross-platform package manager used by NixOS.
*   [`nvr`](./nvr) – Command for controlling Neovim process remotely.
*   [`pg_dump`](./pg_dump)
*   [`psql`](./psql)
*   [`run-mailcap`](./run-mailcap)
*   [`ssh`](./ssh)
*   [`sk`](./sk) – Skim is a fuzzy text selector.
*   [`stack`](./stack) – Cross-platform program for developing Haskell projects.
*   [`tmux`](./tmux) – Terminal multiplexer.
*   [`xdg-open`](./xdg-open)
*   [`yarn`](./yarn) – Package manager for JavaScript ecosystem.
*   [`youtube-dl`](./youtube-dl) – Command-line program to download videos


## Command Line Completion Helpers

Helper functions and scripts for command line completion of Exec commands are
in [`completion`](./completion) directory:

*   [`completion/bash-completion-script-wrapper`](./completion/bash-completion-script-wrapper)
    – Takes standard Bash completion file in a script that provides Command
    Wrapper-style UI on top.  As template it has following type signature:

    ```Dhall
      ∀(opts : Options)
    → Text
    ```

    Type `Options` is defined and documented in
    [`completion/bash-completion-script-wrapper/Options/Type`
    ](./completion/bash-completion-script-wrapper/Options/Type).

    Generated script has following calling convention:

    ```
    COMMAND [--index=NUM] [--shell=SHELL] [-- [WORD ...]]
    ```

    Together with [`completion/wrapper`](./completion/wrapper) it allows
    reusing existing Bash completion script.

*   [`completion/command-wrapper`](./completion/command-wrapper) – Call command
    line completion with Command Wrapper style API. The Dhall function has the
    following type:

    ```Dhall
      ∀(command : Text)
    → ∀(prefixArguments : List Text)
    → ∀(shell : Shell)
    → ∀(index : Natural)
    → ∀(words : List Text)
    → ExecCommand
    ```

    And it generates following command:

    ```
    COMMAND --index=INDEX --shell=SHELL -- [PREFIX_ARGUMENT ...] [WORD ...]
    ```

*   [`completion/optparse-applicative`](./completion/optparse-applicative) –
    Command line interfaces built with [optparse-applicative
    ](https://hackage.haskell.org/package/optparse-applicative) library have
    command line completion baked in.  This function understands its calling
    convention and has the following type:

    ```Dhall
      ∀(command : Text)
    → ∀(prefixArguments : List Text)
    → ∀(shell : Shell)
    → ∀(index : Natural)
    → ∀(words : List Text)
    → ExecCommand
    ```

    And it generates following command:

    ```
    COMMAND --bash-completion-index=INDEX [--bash-completion-enriched]
        [--bash-completion-word=WORD ...]
    ```

*   [`completion/wordlist`](./completion/wordlist) – Simple command line
    completion that uses specified wordlist for completion.  Provided function
    has following type:

    ```Dhall
      ∀(toolset : Text)
    → ∀(wordlist : List Text)
    → ∀(shell : Shell)
    → ∀(index : Natural)
    → ∀(words : List Text)
    → ExecCommand
    ```

    And it generates following command:

    ```Bash
    TOOLSET --no-aliases --silent completion --query --words --pattern=PATTERN
        -- [WORD ...]
    ```

*   [`completion/wrapper`](./completion/wrapper) – Allows implementing command
    line completion via:

    ```
    TOOLSET --no-aliases --silent completion --wrapper --expression=EXPRESSION
        --exec -- [ARGUMENT ...]
    ```

    It expects the `EXPRESSION` to evaluate into a script that takes following
    arugments:

    ```
    SCRIPT --index=INDEX --shell=SHELL -- [WORD ...]
    ```

    For more information see:

    ```
    TOOLSET help [--man] completion
    ```

*   [`completion/scripts`](./completion/scripts) – Bunch of completion scripts
    ready to be used via [`completion/wrapper`](./completion/wrapper).

    See [`completion/scripts`](./completion/scripts) for more information.


## Other Utilities

Some useful utilities can be found in [`utils`](./utils):

*   [`utils/colorOption`](./utils/colorOption) – Convert `ColourOutput` value
    into `--color={always|auto|never}` command line option.  A lot of command
    line options support `--color=WHEN` option, especially GNU applications.

    See [`utils/colorOption`](./utils/colorOption) for a usage example.

    Function [`utils/colourOutputOptions`](./utils/colourOutputOptions) can be
    used if command supports something other than `--color={always|auto|never}`
    option.

*   [`utils/colourOutputOptions`](./utils/colourOutputOptions) – Convert
    `ColourOutput` value into command line options.  Usage example:

     ```Dhall
       colourOutputOptions
         { Always = [ "-C" ], Auto = [] : List Text, Never = [ "-M" ] }
    : ColourOutput → List Text
     ```

*   [`utils/optionalEnvironmentVariables`](./utils/optionalEnvironmentVariables)
    – Convert an `Optional` value into environment variables, or empty list if
    it's `None`.

    ```Dhall
      optionalEnvironmentVariables
        Text
        (λ(_ : Text) → [ { name = "SOME_ENV_VARIABLE", value = _ } ])
    : Optional Text → Environment
    ```

*   [`utils/optionalFlags`](./utils/optionalFlags) – Convert an `Optional`
    value into command line options, or empty list if it's `None`.

    ```Dhall
      optionalFlags [ "--thing" ] [ "--no-thing" ]
    : Optional Bool → List Text
    ```

*   [`utils/optionalOptions`](./utils/optionalOptions) – Convert an `Optional`
    value into command line options, or empty list if it's `None`.

    ```Dhall
      optionalOptions
        Text
        (λ(dir : Text) → [ "--directory=${dir}" ])
        (Some "/a/directory")
    : Optional Text → List Text
    ```

*   [`utils/toShell`](./utils/toShell) – Convert output of
    `TOOLSET exec --print COMMAND` into command that can be executed on command
    line.  See [`utils/toShell`](./utils/toShell) for examples.

*   [`utils/verbosityOptions`](./utils/verbosityOptions) – Convert `Verbosity`
    value into command line options.  Usage example:

    ```Dhall
      verbosityOptions
        { Silent = [ "--quiet" ]
        , Normal = [] : List Text
        , Verbose = [ "--verbose" ]
        , Annoying = [ "--debug" ]
        }
    : Verbosity → List Text
    ```
