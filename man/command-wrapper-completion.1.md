% COMMAND-WRAPPER-COMPLETION(1) Command Wrapper 0.1.0 | Command Wrapper
% Peter Trsko
% 10th April 2019


# NAME

`command-wrapper-completion` -- Command line completion, editor, and IDE
support.


# USAGE

TOOLSET\_COMMAND \[GLOBAL\_OPTIONS] completion \[\--index=*NUM*]
\[\--shell=*SHELL*] \-- [*WORD* ...]

TOOLSET\_COMMAND \[GLOBAL\_OPTIONS] completion \--script \[\--shell=*SHELL*]
\[\--alias=*ALIAS* ...]

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
:   Provide completion or generate script for *SHELL*.  Currently only supported
    value is bash.

*WORD*
:   *WORD*s to complete. In Bash these are the elements of `COMP_WORDS` array.


# COMPLETION SCRIPT OPTIONS

\--script
:   Generate completion script suitable for sourcing in your shell's \*rc file.

\--shell=*SHELL*
:   Provide completion or generate script for *SHELL*.  Currently only supported
    value is bash.

\--alias=*ALIAS*
:   *ALIAS* under which Command Wrapper toolset is also known.  This is usually
    name of a shell alias, e.g. `alias ts=toolset` where ts is an *ALIAS* for
    which we want command line completion to work as well.


# QUERY OPTIONS

\--query
:   Query command line interface.  Useful for editor/IDE integration.

\--subcommands
:   Query all available subcommands.  This includes internal subcommands,
    external subcommands, and subcommand aliases.

\--subcommand-aliases
:   Query available subcommand aliases.

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
