% COMMAND-WRAPPER-COMPLETION(1) Command Wrapper 0.1.0 | Command Wrapper
% Peter Trsko
% 8th January 2019


# NAME

`command-wrapper-completion` -- **TODO**


# USAGE

TOOLSET\_COMMAND \[GLOBAL\_OPTIONS] completion \[\--index=*NUM*] \[\--shell=*SHELL*] \-- [*WORD* ...]

TOOLSET\_COMMAND \[GLOBAL\_OPTIONS] completion \--script \[\--shell=*SHELL*] \[\--alias=*ALIAS* ...]

TOOLSET\_COMMAND \[GLOBAL\_OPTIONS] completion {\--help|-h}

TOOLSET\_COMMAND \[GLOBAL\_OPTIONS] help completion


# DESCRIPTION

**TODO**


# OPTIONS

\--index=*NUM*
:   Position of a *WORD* for which we want completion.  In Bash this is the value
    of `COMP_CWORD` variable.

\--shell=*SHELL*
:   Provide completion or generate script for *SHELL*.  Currently only supported
    value is bash.

\--script
:   Generate completion script suitable for sourcing in your shell's \*rc file.

\--alias=*ALIAS*
:   *ALIAS* under which Command Wrapper toolset is also known.  This is usually
    name of a shell alias, e.g. `alias ts=toolset` where ts is an *ALIAS* for
    which we want command line completion to work as well.

\--help, -h
:   Print this help and exit.  Same as `yx help completion`.

*WORD*
:   *WORD*s to complete. In Bash these are the elements of `COMP_WORDS` array.



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
