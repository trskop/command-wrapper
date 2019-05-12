% COMMAND-WRAPPER-CONFIG(1) Command Wrapper 0.1.0 | Command Wrapper
% Peter Trsko
% 12th May 2019


# NAME

`command-wrapper-config` -- Initialise, query, and update Command Wrapper
toolset configuration.


# USAGE

**TODO: Currently not implemented! With the notable exception of `--help`**

TOOLSET\_COMMAND \[GLOBAL\_OPTIONS] config \[OPTIONS] \[\--] \[*EXPRESSION*]

TOOLSET\_COMMAND \[GLOBAL\_OPTIONS] config \--init \--toolset=*NAME*

TOOLSET\_COMMAND \[GLOBAL\_OPTIONS] config {\--edit|-e} \[\--global|-g|*SUBCOMMAND*]

TOOLSET\_COMMAND \[GLOBAL\_OPTIONS] config {\--help|-h}

TOOLSET\_COMMAND \[GLOBAL\_OPTIONS] help config


# DESCRIPTION

**TODO**


# OPTIONS

**TODO: Currently not implemented! With the notable exception of `--help`**

\--type, -t
:   Print type of final Dhall expression instead of its value.

\--plain, -p
:   Plain output, final Dhall expression must result in one of:

    * `Text`, `Natural`, or `Integer`
    * `List Text`, `List Natural`, or `List Integer`
    * `Optional Text`, or `Optional Natural`, or `Optional Integer`

\--fail-when-none
:   If result expression is an `Optional` value then it has to be `Some`.

\--fail-when-empty
:   If result expression is a `List` value then it has to be non-empty.

\--edit
:   Edit configuration file.  If *SUBCOMMAND* is specified then configuration
    file for that *SUBCOMMAND* is modified instead, and if `--global` then
    toolset configuration file is opened.

\--init \--toolset=*NAME*
:   Initialise a new toolset *NAME*.  It will create symbolic link to Command
    Wrapper executable named *NAME*, initial configuration, and directory for
    its subcommands.

\--help, -h
:   Display help information and exit.  Same as `TOOLSET_COMMAND help config`.

*EXPRESSION*
:   Dhall expression that either queries or updates configuration, depending
    if the `--update` option is present.


# EXIT STATUS

For documentation of generic *EXIT STATUS* codes see `command-wrapper(1)`
manual page section *EXIT STATUS*.  Any *EXIT STATUS* codes specific to this
subcommand will be listed below.


# FILES AND DIRECTORIES

See also `XDG_CONFIG_HOME` in *ENVIRONMENT VARIABLES* section for more
information on how Command Wrapper figures out where to look for this
configuration file.

`${XDG_CONFIG_HOME:-$HOME/.config}/command-wrapper/user-config/index.dhall`
:   **TODO**

`${XDG_CONFIG_HOME:-$HOME/.config}/command-wrapper/user-config/`
:   **TODO**


# ENVIRONMENT VARIABLES

See also `command-wrapper(1)` *ENVIRONMENT VARIABLES* section.  Everything
mentioned there applies to this subcommand as well.

`XDG_CONFIG_HOME`
:   Overrides where this subcommand expects its configuration file.  It follows
    this simple logic:

    * If `XDG_CONFIG_HOME` environment variable is set then the configuration
      file has path:

        ```
        ${XDG_CONFIG_HOME}/command-wrapper/user-config/index.dhall
        ```

    * If `XDG_CONFIG_HOME` environment variable is not set then default value
      is used instead:

        ```
        ${HOME}/.config/command-wrapper/user-config/index.dhall
        ```

    See [XDG Base Directory Specification
    ](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html)
    for more information on rationale behind this.


# SEE ALSO

command-wrapper(1)

* [XDG Base Directory Specification
  ](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html)
* [FZF -- A command-line fuzzy finder](https://github.com/junegunn/fzf)
* [FZF -- Related Projects](https://github.com/junegunn/fzf/wiki/Related-projects)
  page includes links to similar tools that could be used instead of FZF.


# BUGS

<https://github.com/trskop/command-wrapper/issues>
