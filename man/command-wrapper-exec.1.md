% COMMAND-WRAPPER-EXEC(1) Command Wrapper 0.1.0 | Command Wrapper
% Peter Trsko
% 23nd December 2018


# NAME

`command-wrapper-exec` -- Generate subcommand skeleton for specific
Command Wrapper environment, i.e. toolset.


# USAGE

TOOLSET\_COMMAND \[GLOBAL\_OPTIONS] exec {-l|\--ls}

TOOLSET\_COMMAND \[GLOBAL\_OPTIONS] exec COMMAND [\--] \[EXTRA\_COMMAND\_ARGUMENTS]


# DESCRIPTION

**TODO**


# OPTIONS

-l, \--ls
:   List available *COMMAND*s.

COMMAND
:   Command name as it is specified in configuration file.  If available then
    it is executed.  Any *EXTRA_COMMAND_ARGUMENTS* are passed to it.


# EXIT STATUS

TODO


# FILES

`${XDG_CONFIG_HOME:-$HOME/.config}/command-wrapper/command-wrapper-exec.dhall`
:   Configuration file specifies *COMMAND*s that can be invoked.  Command
    configuration is actually a function with a name associated with it.

    See also `XDG_CONFIG_HOME` in *ENVIRONMENT VARIABLES* section for more
    information on how Command Wrapper figures out where to look for this
    configuration file.


# ENVIRONMENT VARIABLES

See also `command-wrapper(1)` *ENVIRONMENT VARIABLES* section.  Everything
mentioned there applies to this subcommand as well.

`XDG_CONFIG_HOME`
:   Overrides where this subcommand expects its configuration file.  It follows
    this simple logic:

    * If `XDG_CONFIG_HOME` environment variable is set then the configuration
      file has path:

        ```
        ${XDG_CONFIG_HOME}/command-wrapper/command-wrapper-exec.dhall
        ```

    * If `XDG_CONFIG_HOME` environment variable is not set then default value
      is used instead:

        ```
        ${HOME}/.config/command-wrapper/command-wrapper-exec.dhall
        ```

    See [XDG Base Directory Specification
    ](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html)
    for more information on rationale behind this.


# SEE ALSO

command-wrapper(1)

* [XDG Base Directory Specification
  ](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html)


# BUGS

<https://github.com/trskop/command-wrapper/issues>
