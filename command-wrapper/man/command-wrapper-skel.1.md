% COMMAND-WRAPPER-SKEL(1) Command Wrapper 0.1.0 | Command Wrapper
% Peter Trsko
% 2nd June 2019


# NAME

`command-wrapper-skel` - Generate subcommand or configuration file skeleton
for specific Command Wrapper environment, i.e. toolset.


# USAGE

TOOLSET\_COMMAND \[GLOBAL\_OPTIONS] skel \[\--language=LANGUAGE|-l LANGUAGE] SUBCOMMAND

TOOLSET\_COMMAND \[GLOBAL\_OPTIONS] skel {\--help|-h}

TOOLSET\_COMMAND \[GLOBAL\_OPTIONS] help skel


# DESCRIPTION

Purpose of this Command Wrapper subcommand is to allow users to rapidly create
or prototype new subcommands.

Command Wrapper subcommand can be written in any language user chooses.  In the
end it is just an executable that follows Command Wrapper's *SUBCOMMAND
PROTOCOL*, which is documented in a separate manual page
`command-wrapper-subcommand-protocol(7)`.  At the moment this subcommand
provides skeletons only for a alimited number of languages, see
`--language=`*LANGUAGE* option for more details.


# OPTIONS

\--language=*LANGUAGE*, \--language *LANGUAGE*, -l *LANGUAGE*
:   Choose programming language of the new subcommand.  Currently only
    following values are supported:

    * *haskell* (default)
    * *bash*
    * *dhall* (configuration files)

\--parents, -p
:   Create parent directories if they do not exist.

\--\[no-]edit, -e, -E
:   Open, or not, the created file in an editor afterwards.  Options `-e` and
    `-E` are equivalent to `--edit` and `--no-edit`, respectively.

\--help, -h
:   Display help information and exit.  Same as `TOOLSET_COMMAND help skel`.

`SUBCOMMAND`
:   Name of the new subcommand.  Where and how the source code or executable
    file will be named is configurable.  By default it is:

    ```
    ${HOME}/.local/lib/${toolset}/${toolset}-${subcommand}
    ```


# EXIT STATUS

For documentation of generic *EXIT STATUS* codes see `command-wrapper(1)`
manual page section *EXIT STATUS*.  Any *EXIT STATUS* codes specific to this
subcommand will be listed below.

`3`
:   Cannot create target file.  This can happen for multiple reasons:

    *   Target directory doesn't exist and `--parens` option wasnt specified.
    *   Target file exists and we don't want to overwrite it.


# FILES

`${XDG_CONFIG_HOME:-$HOME/.config}/${toolset}/command-wrapper-skel.dhall`
:   Configuration file specifies templates for individual *LANGUAGE*s, how the
    new *SUBCOMMAND* files will be named, and where they will be stored.

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
        ${XDG_CONFIG_HOME}/${toolset}/command-wrapper-skel.dhall
        ```

    * If `XDG_CONFIG_HOME` environment variable is not set then default value
      is used instead:

        ```
        ${HOME}/.config/${toolset}/command-wrapper-skel.dhall
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
