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

`COMMAND_WRAPPER_INVOKE_AS`
:   This value overrides the name under which `command-wrapper` command was
    executed.

    Lets assume that `$HOME/bin/foo` is a symlink to real command-wrapper
    binary and that `$HOME/bin` is in the `PATH`.  If we run following command:

    ```
    COMMAND_WRAPPER_INVOKE_AS=bar foo
    ```

    Then Command Wrapper will behave as if it was invoked as `bar`.   This is
    useful for debugging toolsets, and for developing new one.  More
    importantly this allows subcommands to call toolsets reliably.  Without
    this mechanism subcommands would either need toolsets to always be in
    `PATH` or we would need to pass toolset specific symlink/binary in an
    environment variable.  See `command-wrapper-subcommand-protocol(7)` for
    more details on how subcommands are invoked.


# SEE ALSO

TODO


# BUGS

<https://github.com/trskop/command-wrapper/issues>
