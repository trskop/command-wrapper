% COMMAND-WRAPPER(1) Command Wrapper 0.1.0 | Command Wrapper
% Peter Trsko
% 23nd December 2018


# NAME

`command-wrapper` -- Command to build Git-style command-line toolsets on top
of.


# USAGE

TOOLSET\_COMMAND \[GLOBAL\_OPTIONS] SUBCOMMAND \[\--] \[SUBCOMMAND\_ARGUMENTS]

TOOLSET\_COMMAND \[GLOBAL\_OPTIONS] help \[SUBCOMMAND]

TOOLSET\_COMMAND \[GLOBAL\_OPTIONS] config \[SUBCOMMAND] \[CONFIG\_OPTIONS]

TOOLSET\_COMMAND \[GLOBAL\_OPTIONS] completion \[COMPLETION\_OPTIONS]

TOOLSET\_COMMAND {\--help|-h}


# DESCRIPTION

Some command line applications with a lot of commands try to avoid polluting
`$PATH` with all of them.  One of the approaches to this is to have one top
level command exposed and the rest is implemented as subcommands.  Subcommands
are either internal functions or external commands (standalone executables).
Example of such application is Git which uses mix of internal subcommands and
external subcommand.

In general such toolset top level command has syntax like this:

    TOOLSET_COMMAND [GLOBAL_OPTIONS] SUBCOMMAND [SUBCOMMAND_ARGUMENTS]

This package provides universal top-level command, that can be named as
required, and API for subcommands.  Subcommands may be written in any language,
they just need to be executable files that respect the subcommand API.


# GLOBAL OPTIONS

\--verbosity=*VERBOSITY*
:   Specify how annoying the output of the command should be.  Possible values
    of *VERBOSITY* are:

    * *silent* -- Don't print any messages, not even error messages.  In case of
      an error the command should still indicate that fact with a proper
      *EXIT STATUS*.
    * *normal* -- Print only important messages.
    * *verbose* -- Print anything that comes into mind.
    * `annoying` -- Print debugging/tracing information.

    Be aware that *VERBOSITY* should not affect output that is part interactive
    session with the user.

\--silent
:   Same as `--verbosity=silent` and `--quiet`.

\--quiet
:   Same as `--verbosity=silent` and `--silent`.

\--verbose
:   Same as `--verbosity=verbose`.

-v
:   Increment verbosity by one level; can be repeated.

\--color[=*WHEN*], \--colour[=*WHEN*]
:   Colourise output; *WHEN* can be one of `always` (default if *WHEN* is omitted),
    `auto`, or `never`.  See also `NO_COLOR` in *ENVIRONMENT VARIABLES*
    section.

\--no-color, \--no-colour
:   Same as `--colour=never`.  See also `NO_COLOR` in *ENVIRONMENT VARIABLES*
    section.

\--no-aliases
:   Ignore *SUBCOMMAND* aliases.  This is useful when used from e.g. scripts to
    avoid issues with user defined aliases interfering with how the script
    behaves.  **TODO: Not yet implemented!**

-C *DIRECTORY*, \--change-directory=*DIRECTORY*
:   Change working directory before doing anything, especially executing a
    subcommand.  **TODO: Not yet implemented!**

-h, --help
:   Print toolset (*TOOLSET_COMMAND*) help information and exit.  Using `help`
    subcommand without arguments gives the same result.


# SUBCOMMANDS

There are two kind of subcommands, external, and internal.  Internal
subcommands are:

* *help* -- Print help information for top-level toolset command or a specified
  *SUBCOMMAND*.

* *config* -- Currently not implemented.

* *completion* -- Provides shell completion.  Currently only partially
  implemented for Bash shell.

Some external subcommands are bundled with Command Wrapper itself:

* *cd* -- Start a new subshell / Tmux window / terminal emulator in a selected
  directory.  See `command-wrapper-cd(1)` for more details.

* *exec* -- Execute predefined command with a user specified environment.  See
  `command-wrapper-exec(1)` for more details.

* *skel* -- Generate subcommand skeleton for specific Command Wrapper
  environment, i.e. toolset.  See `command-wrapper-skel(1)` for more details.


# EXIT STATUS

`0`
:   If everything went OK.  Executable `command-wrapper` returns this status
    only when it handled the subcommand internaly.  In case of it invoking an
    external command this will be returned by the subcommand itself.

`1`
:   If Command Wrapper or its subcommand has encounter one of the following
    issues:

        * Unable to parse command line arguments/options.
        * Unable to parse configuration file, or if there was a type error in
          a configuration file.
        * Required configuration file is missing.

`2`
:   Returned by a subcommand if the Command Wrapper environment wasn't set up
    properly.  In other words *SUBCOMMAND PROTOCOL* was violated.

    This can indicate a version mismatch between Command Wrapper installation,
    and *subcommand*/*toolset* installation.

*OTHER*
:   Subcommands are free to use other *EXIT STATUS* codes if there is no
    *EXIT STATUS* defined for that purpose.  See also *SUBCOMMAND PROTOCOL*.


# FILES AND DIRECTORIES

`${XDG_CONFIG_HOME:-$HOME/.config}/command-wrapper/default.dhall`
:   Top-level command wrapper configuration file.  It mostly provides defaults
    that can be overriden by `GLOBAL_OPTIONS`, additional help messages, and
    alias definitions.

    See also `XDG_CONFIG_HOME` in *ENVIRONMENT VARIABLES* section to better
    understand how the location of the configuration file is determined.

`${XDG_CONFIG_HOME:-$HOME/.config}/${toolset}/default.dhall`
:   Same as already mentioned `.../command-wrapper/default.dhall`.  However
    this one is used by a specific `toolset`, and it is applied on top of the
    `command-wrapper` one.  Configuration for a specific *toolset* is
    `.../command-wrapper/default.dhall` unless it is overriden in
    `.../${toolset}/default.dhall`.

    See also `XDG_CONFIG_HOME` in *ENVIRONMENT VARIABLES* section to better
    understand how the location of the configuration file is determined.


`${XDG_CONFIG_HOME:-$HOME/.config}/${toolset}/${toolset}-${subcommand}.dhall`
:   Subcommand specific configuration file.  There is not much we can say about
    them, since every subcommand can have its own definition.

    See also:

    * `command-wrapper-subcommand-protocol(7)` for more details on how
      subcommands use their configuration files.
    * `XDG_CONFIG_HOME` in *ENVIRONMENT VARIABLES* section to better understand
      how the location of the configuration file is determined.

`${XDG_CONFIG_HOME:-$HOME/.local}/share/man/`
:   Command Wrapper manual pages are installed into this directory so that
    standard `man` command is able to find them.

`${HOME}/.local/lib/command-wrapper/command-wrapper`
:   Default installation path for `command-wrapper` executable.  It's not
    expected to be in `$PATH`, hence the use of libexec-style directory.
    See also description of `${HOME}/.local/lib/command-wrapper/`.

`${HOME}/.local/lib/command-wrapper/command-wrapper-${subcommand}`
:   Default installation path for a common/global *subcommand* executable.  It's
    not expected to be in `$PATH`, hence the use of libexec-style directory.
    See also description of `${HOME}/.local/lib/command-wrapper/`.

`${HOME}/.local/lib/command-wrapper/`
:   Directory where `command-wrapper` executable is installed along with its
    external subcommands `command-wrapper-cd`, `command-wrapper-exec`,
    `command-wrapper-skel`, and possibly others.

    Any *toolset* command that is created as an alias for `command-wrapper`
    will search this directory if it won't be able to find a more specific
    command in `${HOME}/.local/lib/${toolset}/`

    The idea behind choosing this directory came from *XDG Base Directory
    Specification* and some existing tools that are already using it.  *XDG
    Base Directory Specification* defines `${HOME}/.local/share` as a default
    data directory, which resembles standard `/usr/local/share` directory.
    Systemd have built on top of that by assuming that `${HOME}/.local`
    directory hierarchy should be similar to standard `/usr/local` hierarchy.
    This is described in [Systemd's File Hierarchy
    ](https://www.freedesktop.org/software/systemd/man/file-hierarchy.html),
    or in `file-hierarchy(7)` on systems with systemd.  This approach can be
    further generalised by saying that `${HOME}/.local/` should have the same
    structure as `/usr/local/`, which some tools do.

    Few examples of other tools using this hierarchy:

    * Cabal and Stack (`${HOME}/.local/bin/`).
    * Pip (`${HOME}/.local/{include,lib,...}/python${python_version}/`). See
      also [PEP 370 -- Per user site-packages directory
      ](https://www.python.org/dev/peps/pep-0370/) for more details.

`${HOME}/.local/lib/${toolset}/${toolset}-${subcommand}`
:   Default installation path for a *toolset subcommand* executable.  It's
    not expected to be in `$PATH`, hence the use of libexec-style directory.
    See also description of `${HOME}/.local/lib/command-wrapper/`.

`${HOME}/.local/lib/${toolset}/`
:   This is the generalisation of the idea behind
    `${HOME}/.local/lib/${toolset}/` directory.   Whenever a new *toolset* is
    created, as a symlink to
    `${HOME}/.local/lib/command-wrapper/command-wrapper` executable, it will be
    looking for its (external) subcommands in this directory.  Subcommands are
    expected to be executable and have following absolute path:

    ```
    ${HOME}/.local/lib/${toolset}/${toolset}-${subcommand}
    ```

    If *toolset* is unable to find a subcommand `${toolset}-${subcommand}`, in
    this directory, then it will try to look for `command-wrapper-${subcommand}`
    in `${HOME}/.local/lib/command-wrapper/` directory.  This way toolsets can
    reuse common subcommands.


# ENVIRONMENT VARIABLES

`XDG_CONFIG_HOME`
:   Overrides where Command Wrapper looks for configuration files.  Loading
    *toolset* configuration file uses following logic:

    * If `XDG_CONFIG_HOME` environment variable is set then the configuration
      file has path:

        ```
        ${XDG_CONFIG_HOME}/${toolset}/default.dhall
        ```

    * If `XDG_CONFIG_HOME` environment variable is not set then default value
      is used instead:

        ```
        ${HOME}/.config/${toolset}/default.dhall
        ```

    For subcommand configuration files it is:

    * If `XDG_CONFIG_HOME` environment variable is set then the configuration
      file has path:

        ```
        ${XDG_CONFIG_HOME}/${toolset}/${toolset}-${subcommand}.dhall
        ```

    * If `XDG_CONFIG_HOME` environment variable is not set then default value
      is used instead:

        ```
        ${HOME}/.config/${toolset}/${toolset}-${subcommand}.dhall
        ```

    See [XDG Base Directory Specification
    ](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html)
    for more information on rationale behind this.

`NO_COLOR`
:   This environment variable is an informal standard which is available
    online at <https://no-color.org>. The standard states:

    > Accepting the futility of trying to reverse this trend, an informal
    > standard is hereby proposed:
    >
    > All command-line software which outputs text with ANSI color added
    > should check for the presence of a `NO_COLOR` environment variable
    > that, when present (regardless of its value), prevents the addition of
    > ANSI color.

    Command Wrapper treats @NO_COLOR@ as a default value.  It can be overridden
    by `colourOutput` property in *toolset* configuration file (`default.dhall`)
    and/or using command line option(s).

    Alternatively, following command can be used to temporarily disable
    `NO_COLOR`:

    ```
    env -u 'NO_COLOR' TOOLSET_COMMAND [GLOBAL_OPTIONS] SUBCOMMAND [SUBCOMMAND_ARGUMENTS]
    ```

`COMMAND_WRAPPER_INVOKE_AS`
:   This value overrides the name under which `command-wrapper` command was
    executed.

    Lets assume that `$HOME/bin/foo` is a symlink to real command-wrapper
    binary and that `$HOME/bin` is in the `PATH`.  If we run following command:

    ```
    COMMAND_WRAPPER_INVOKE_AS=bar foo
    ```

    Or if your shell doesn't support the above syntax:

    ```
    env COMMAND_WRAPPER_INVOKE_AS=bar foo
    ```

    Then Command Wrapper will behave as if it was invoked as `bar`.   This is
    useful for debugging toolsets, and for developing new one.  More
    importantly this allows subcommands to call toolsets reliably.  Without
    this mechanism subcommands would either need toolsets to always be in
    `PATH` or we would need to pass *toolset* specific symlink/binary in an
    environment variable.

    The most reliable way how to invoke Command Wrapper in a subcommand, that
    is implemented as Bash script, is:

    ```
    function toolset() {
       COMMAND_WRAPPER_INVOKE_AS=${COMMAND_WRAPPER_NAME} ${COMMAND_WRAPPER_EXE} "$@"
    }
    ```

    See also `command-wrapper-subcommand-protocol(7)` for more details on how
    subcommands are invoked.


# SUBCOMMAND PROTOCOL

Command Wrapper follows a specific calling convention for external subcommands.
Subcommands are required to follow a specific protocol when invoked.  This
protocol is described in `command-wrapper-subcommand-protocol(7)` manual page.


# SEE ALSO

command-wrapper-cd(1), command-wrapper-exec(1), command-wrapper-skel(1),
command-wrapper-subcommand-protocol(7)

* [XDG Base Directory Specification
  ](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html)


# BUGS

<https://github.com/trskop/command-wrapper/issues>
