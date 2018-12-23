% COMMAND-WRAPPER(1) Command Wrapper 0.1.0 | Command Wrapper
% Peter Trsko
% 22nd December 2018


# NAME

`command-wrapper` -- Command to build Git-style command-line toolsets on top
of.


# DESCRIPTION

TODO


# USAGE

command-wrapper \[GLOBAL\_OPTIONS] SUBCOMMAND \[SUBCOMMAND\_ARGUMENTS]

command-wrapper \[GLOBAL\_OPTIONS] help \[SUBCOMMAND]

command-wrapper \[GLOBAL\_OPTIONS] config \[SUBCOMMAND] \[CONFIG\_OPTIONS]

command-wrapper \[GLOBAL\_OPTIONS] completion \[COMPLETION\_OPTIONS]

command-wrapper {\--help|-h}


# GLOBAL OPTIONS

TODO


# SUBCOMMANDS

There are two kind of subcommands, external, and internal.  Internal
subcommands are:

* *help* -- Print help information for top-level toolset command or a specified
  *SUBCOMMAND*.
* *config* -- Currently not implemented.
* *completion* -- Provides shell completion.  Currently only partially
  implemented for Bash shell.

Some external subcommands are bundled with Command Wrapper itself:

* *cd*
* *exec*
* *skel*


# EXIT STATUS

TODO


# FILES

TODO


# ENVIRONMENT VARIABLES

TODO


# SEE ALSO

TODO


# BUGS

<https://github.com/trskop/command-wrapper/issues>
