% COMMAND-WRAPPER-HELP(1) Command Wrapper 0.1.0 | Command Wrapper
% Peter Trsko
% 26th June 2019


# NAME

`command-wrapper-help` -- Display help message or manual page for Command
Wrapper or one of its subcommands.


# USAGE

TOOLSET\_COMMAND \[GLOBAL\_OPTIONS] help \[*SUBCOMMAND*]

TOOLSET\_COMMAND \[GLOBAL\_OPTIONS] help \--man \[*SUBCOMMAND*|*TOPIC*]

TOOLSET\_COMMAND \[GLOBAL\_OPTIONS] help \--aliases

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

\--aliases
:   List and describe available aliases.

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