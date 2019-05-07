% COMMAND-WRAPPER-COMPLETION(1) Command Wrapper 0.1.0 | Command Wrapper
% Peter Trsko
% 7th May 2019


# NAME

`command-wrapper-completion` -- Command line completion, editor, and IDE
support.


# USAGE

TOOLSET\_COMMAND \[GLOBAL\_OPTIONS] completion \[\--index=*NUM*]
\[\--shell=*SHELL*] \[\--subcommand=*SUBCOMMAND*] \-- [*WORD* ...]

TOOLSET\_COMMAND \[GLOBAL\_OPTIONS] completion \--script \[\--shell=*SHELL*]
\[\--subcommand=*SUBCOMMAND* \--alias=*ALIAS* \[\--alias=*ALIAS* ...]|\--alias=*ALIAS*
...]

TOOLSET\_COMMAND \[GLOBAL\_OPTIONS] completion \--library \[\--shell=*SHELL*]

TOOLSET\_COMMAND \[GLOBAL\_OPTIONS] completion \--query \[QUERY\_OPTIONS]

TOOLSET\_COMMAND \[GLOBAL\_OPTIONS] completion {\--help|-h}

TOOLSET\_COMMAND \[GLOBAL\_OPTIONS] help completion


# DESCRIPTION

This subcommand provides support for command line completion (also known as TAB
completion).  At the moment only Bash is supported.

Another purpose of this command is editor and IDE support. It provides
interface for querying Command Wrapper's command line interface (CLI).


# COMMAND LINE COMPLETION OPTIONS

\--index=*NUM*
:   Position of a *WORD* for which we want completion.  In Bash this is the value
    of `COMP_CWORD` variable.

\--shell=*SHELL*
:   Provide completion or generate script for *SHELL*. Supported *SHELL* values
    are: *bash* and *fish*.

\--subcommand=*SUBCOMMAND*
:   Do command line completion for a *SUBCOMMAND* instead.  This is useful for
    example when debugging command line completion of a *SUBCOMMAND*.

    ```
    $ TOOLSET_COMMAND completion --subcommand=version -- --he
    --help
    ```

    Value of *SUBCOMMAND* can be any internal subcommand, external subcommand,
    or an alias to one of those.

*WORD*
:   *WORD*s to complete. In Bash these are the elements of `COMP_WORDS` array.


# COMPLETION SCRIPT OPTIONS

\--script
:   Generate completion script suitable for sourcing in your shell's \*rc file.

\--shell=*SHELL*
:   Provide completion or generate script for *SHELL*.  Supported *SHELL* values
    are: *bash* and *fish*.

\--subcommand=*SUBCOMMAND*
:   Generate completion script for a *SUBCOMMAND* instead of the whole toolset.
    At least one instance of `--alias=`*ALIAS* has to be specified:

    ```
    TOOLSET_COMMAND completion --script --subcommand=this --alias=this
    ```

    Completion will be generated for `this` command, therefore there should
    also be an alias for it defined in e.g. `.bashrc`:

    ```
    alias this='TOOLSET_COMMAND this'
    ```

\--alias=*ALIAS*
:   *ALIAS* under which Command Wrapper toolset is also known.  This is usually
    name of a shell alias, e.g. `alias ts=toolset` where ts is an *ALIAS* for
    which we want command line completion to work as well.


# SUBCOMMAND LIBRARY OPTIONS

\--library
:   Print a library to standard output that can be used by a subcommand.  In
    a subcommand implemented in Bash we can include a support library using:

    ```
    source <(COMMAND_WRAPPER_INVOKE_AS="${COMMAND_WRAPPER_NAME}" "${COMMAND_WRAPPER_EXE}" completion --library --shell=bash)
    ```

    Library itself is documented.  One can read through it by just printing it:

    ```
    TOOLSET completion --library [--shell=SHELL]
    ```

    Or piping it to pager:

    ```
    TOOLSET completion --library [--shell=SHELL] | less
    ```

    On Debian we can also use `sensible-pager` command instead of directly
    calling specific one.  Commands like `bat` or other `cat`-like tool with
    syntax highlighting support are also a great choice:

    ```
    TOOLSET completion --library --shell=bash | bat --language bash
    ```

\--shell=*SHELL*
:   Print library for *SHELL*.  Currently only supported value is *bash*.


# QUERY OPTIONS

\--query
:   Query command line interface.  Useful for editor/IDE integration.

\--subcommands
:   Query all available subcommands.  This includes internal subcommands,
    external subcommands, and subcommand aliases.  See also global option
    `--no-aliases` in `command-wrapper(1)`.

\--subcommand-aliases
:   Query available subcommand aliases.  See also global option `--no-aliases`
    in `command-wrapper(1)`.

\--supported-shells
:   Query shells supported by command line completion.  (Currently only Bash is
    supported.)

\--verbosity-values*
:   *Query possible *VERBOSITY* values.  These can be set using global
    `--verbosity=VERBOSITY` option, or are passed down to subcommands via
    `COMMAND_WRAPPER_VERBOSITY` environment variable.

    See `TOOLSET_COMMAND help` or `command-wrapper(1)` for more information on
    `--verbosity=VERBOSITY` option, and `command-wrapper-subcommand-protocol(7)`
    regarding `COMMAND_WRAPPER_VERBOSITY` environment variable.

\--colo[u]r-values*
:   *Query possible *WHEN* colour output values.  These can be set using global
    `--colo[u]r=WHEN` option, or are passed down to subcommands via
    `COMMAND_WRAPPER_COLOUR` environment variable.

    See `TOOLSET_COMMAND help` or `command-wrapper(1)` for more information on
    `--colo[u]r=WHEN` option, and `command-wrapper-subcommand-protocol(7)`
    regarding `COMMAND_WRAPPER_COLOUR` environment variable.


# OTHER OPTIONS

\--help, -h
:   Print this help and exit.  Same as `TOOLSET_COMMAND help completion`.


# EXIT STATUS

See `command-wrapper(1)` manual page section *EXIT STATUS*.


# FILES AND DIRECTORIES

See `command-wrapper(1)` manual page section *FILES AND DIRECTORIES*.


# ENVIRONMENT VARIABLES

See `command-wrapper(1)` manual page section *ENVIRONMENT VARIABLES*.


# BASH CONFIGURATION

This subcommand can generate Bash completion script, which can be sourced in
`~/.bashrc` file:

```
# shellcheck source=/dev/null
source <(toolset completion --script --shell=bash)
```

Where `toolset` is the name under which `command-wrapper` is used.  If any
aliases for `toolset` are used we can provide completion for them as well:

```
alias ts=toolset
# shellcheck source=/dev/null
source <(toolset completion --script --shell=bash --alias='ts')
```


# SEE ALSO

command-wrapper(1), command-wrapper-subcommand-protocol(7)


# BUGS

<https://github.com/trskop/command-wrapper/issues>
