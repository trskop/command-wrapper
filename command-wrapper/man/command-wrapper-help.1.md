% COMMAND-WRAPPER-HELP(1) Command Wrapper 0.1.0 | Command Wrapper
% Peter Trsko
% 17th April 2020


# NAME

`command-wrapper-help` - Display help message or manual page for Command
Wrapper or one of its subcommands.


# USAGE

TOOLSET\_COMMAND \[GLOBAL\_OPTIONS] help \[*SUBCOMMAND*]

TOOLSET\_COMMAND \[GLOBAL\_OPTIONS] help \--man \[*SUBCOMMAND*|*TOPIC*]

TOOLSET\_COMMAND \[GLOBAL\_OPTIONS] help \--search {*TOPIC*|*REGEXP*}

TOOLSET\_COMMAND \[GLOBAL\_OPTIONS] help {\--list|\--tree|\--aliases}

TOOLSET\_COMMAND \[GLOBAL\_OPTIONS] help {\--help|-h}

TOOLSET\_COMMAND \[GLOBAL\_OPTIONS] help help


# DESCRIPTION

Display help information either for `TOOLSET_COMMAND`, or for a `SUBCOMMAND`.
Help message is printed to standard output, and may contains ANSI escape codes
if coloured output is allowed.  See `command-wrapper(1)` for more information
on colour settings.


# OPTIONS

\--man \[*SUBCOMMAND*|*TOPIC*]
:   Show manual page for *SUBCOMMAND*|*TOPIC* instead of short help message.  If
    neither is specified then manual page for current toolset is displayed.

\--search {*TOPIC*|*REGEXP*}
:   Search documentation for specified *TOPIC* or *REGEXP*.

    For this to work we need to run `mandb` in a way that will index Command
    Wrapper manual pages:

    1.  Get Command Wrapper `MANPATH` (substitute `TOOLSET` to your toolset
        name):

        ```Bash
        TOOLSET config --get \
        | dhall-filter \
            --let=Prelude="$(TOOLSET completion --library --dhall=prelude --import)" \
            'Prelude.Text.concatSep ":" input.manPath'
        ```

    2.  Run `mandb` (substitute `COMMAND_WRAPPER_MANPATH` with the value we got
        in step 1):

        ```Bash
        mandb --user-db COMMAND_WRAPPER_MANPATH
        ```

\--list
:   List and describe all available subcommands including aliases.

\--tree
:   List and describe all available subcommands including aliases in tree
    representation using '.' character as a separator.

    Let's say that we have following commands reported by `--list`:

    ```
    build.back-end
    build.back-end.locally
    build.back-end.locally
    build.back-end.remotely
    build.back-end.remotely
    build.front-end
    completion
    config
    help
    ```

    These would be displayed as a following tree:

    ```
    ├── build
    │   ├── back-end
    │   │   ├── locally
    │   │   └── remotely
    │   └── front-end
    │       ├── locally
    │       └── remotely
    ├── completion
    ├── config
    └── help
    ```

\--aliases
:   List and describe available aliases, otherwise it's the same as `--list`.

\--help, -h
:   Display help information and exit.  Same as `TOOLSET_COMMAND help help`.

*SUBCOMMAND*
:   *SUBCOMMAND* for which to print help.  If *SUBCOMMAND* is an alias then it is
    correctly resolved before displaying help message.


# EXIT STATUS

See `command-wrapper(1)` manual page section *EXIT STATUS*.


# FILES AND DIRECTORIES

See `command-wrapper(1)` manual page section *FILES AND DIRECTORIES*.


# ENVIRONMENT VARIABLES

See `command-wrapper(1)` manual page section *ENVIRONMENT VARIABLES*.


# SEE ALSO

command-wrapper(1)


# BUGS

<https://github.com/trskop/command-wrapper/issues>
