% COMMAND-WRAPPER-VERSION(1) Command Wrapper 0.1.0 | Command Wrapper
% Peter Trsko
% 22nd June 2019


# NAME

`command-wrapper-version` - Display version information.


# USAGE

TOOLSET\_COMMAND \[GLOBAL\_OPTIONS] version \[\--dhall|\--shell=*FILE*]
\[\--output=*SHELL*]

TOOLSET\_COMMAND \[GLOBAL\_OPTIONS] {\--version|-V}

TOOLSET\_COMMAND \[GLOBAL\_OPTIONS] version {\--help|-h}

TOOLSET\_COMMAND \[GLOBAL\_OPTIONS] help version


# DESCRIPTION

Display version information.


# OPTIONS

\--dhall
:   Print version information in Dhall format.

\--shell=*SHELL*
:   Print version information in format suitable for *SHELL*.  Supported *SHELL*
    values are: *bash*, *fish*, and *zsh*.

\--output=*FILE*, -o *FILE*
:   Write optput into FILE instead of standard output.

\--help, -h
:   Display help information and exit.  Same as `TOOLSET_COMMAND help version`.


# EXIT STATUS

See `command-wrapper(1)` manual page section *EXIT STATUS*.


# FILES AND DIRECTORIES

See `command-wrapper(1)` manual page section *FILES AND DIRECTORIES*.


# ENVIRONMENT VARIABLES

See `command-wrapper(1)` manual page section *ENVIRONMENT VARIABLES*.


# EXAMPLES

Using version information in Bash:

```
source <(TOOLSET_COMMAND version --shell=bash)
```

Same in Fish shell:

```
TOOLSET_COMMAND version --shell=fish | source
```


# SEE ALSO

command-wrapper(1)


# BUGS

<https://github.com/trskop/command-wrapper/issues>
