% COMMAND-WRAPPER-CONFIG(1) Command Wrapper 0.1.0 | Command Wrapper
% Peter Trsko
% 19th May 2019


# NAME

`command-wrapper-config` -- Initialise, query, and update Command Wrapper
toolset configuration.


# USAGE

TOOLSET\_COMMAND \[GLOBAL\_OPTIONS] config \--dhall
\[\--\[no-]allow-imports|\--\[no-]alpha|\--\[no-]annotate|\--\[no-]type]

TOOLSET\_COMMAND \[GLOBAL\_OPTIONS] config \--dhall-repl

TOOLSET\_COMMAND \[GLOBAL\_OPTIONS] config \--dhall-diff *EXPRESSION* *EXPRESSION*

TOOLSET\_COMMAND \[GLOBAL\_OPTIONS] config \--dhall-hash

TOOLSET\_COMMAND \[GLOBAL\_OPTIONS] config \--init \[\--toolset=*NAME*]

TOOLSET\_COMMAND \[GLOBAL\_OPTIONS] config {\--help|-h}

TOOLSET\_COMMAND \[GLOBAL\_OPTIONS] help config

**TODO: Following usages aren't currently implemented!**

TOOLSET\_COMMAND \[GLOBAL\_OPTIONS] config \[OPTIONS] \[\--] \[*EXPRESSION*]

TOOLSET\_COMMAND \[GLOBAL\_OPTIONS] config {\--edit|-e} \[\--global|-g|*SUBCOMMAND*]


# DESCRIPTION

Command Wrapper's configuration swiss army knife.  On of its many purposes is
to provide Dhall functionality without the need to install external tools.


# OPTIONS

\--dhall
:   Run as interpreter for the Dhall language.

\--\[no-]allow-imports
:   Controls whether imports in the input expression are allowed or not.

\--\[no-]alpha
:   Perform α-normalisation of Dhall expression.

\--\[no-]annotate
:   Add a type annotation to the output.

\--dhall-repl
:   Interpret Dhall expressions in a REPL.

\--dhall-diff
:   Render the difference between the normal form of two Dhall expressions.

\--dhall-hash
:   Compute semantic hashes for Dhall expressions.

\--init
:   Initialise configuration of a toolset.  This includes symlinking command
    wrapper under the toolset's name.  See also `--toolset=`*NAME* option.

 \--toolset=*NAME*
:   When specified allong with `--init` then configuration for toolset *NAME*
    is initialised.  Alternatively `COMMAND_WRAPPER_INVOKE_AS=`*NAME* can be
    used.  See `command-wrapper(1) section *ENVIRONMENT VARIABLES* for details.

\--help, -h
:   Display help information and exit.  Same as `TOOLSET_COMMAND help config`.

*EXPRESSION*
:   Dhall expression.

**TODO: Following options aren't currently implemented!**

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
