% COMMAND-WRAPPER(1) Command Wrapper 0.1.0 | Command Wrapper
% Peter Trsko
% 5th January 2019


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

Many UNIX/Linux users create their own ad-hoc tools that serve a specific need.
This need may be specific to their use-case, their job, or just a one-off.
Core idea of Command Wrapper is to provide a structure for creating such
scripts as fast as possible, and with a reasonable user experience right away.

Another thing that comes from having a lot of tools is that they are scattered
all over the place.  Command Wrapper sidesteps this by hiding them from `$PATH`
by using similar approach as e.g. Git.  Command Wrapper subcommands are either
internal functions or external commands (standalone executables).  It allows
you to define what is called *toolset*.  A symbolic link to it's main
executable, which reuses all the basic machinery of Command Wrapper, but has
it's own name-space for subcommands.

In general such *TOOLSET_COMMAND* has syntax like this:

    TOOLSET_COMMAND [GLOBAL_OPTIONS] SUBCOMMAND [SUBCOMMAND_ARGUMENTS]

Multiple toolsets can easily coexist on the same machine.  It usually makes
sense to have one for personal tooling, and one for work tooling.

First subcommand that was introduced was `help`, obviously, but the one right
after that was `skel`.  Which allows you to create a new subcommand skeleton,
see `command-wrapper-skel(1)` for more details.  Subcommand can be written in
any language user chooses.  It just needs to be an executable, and follow
Command Wrapper's *SUBCOMMAND PROTOCOL*, which is documented in a separate
manual page `command-wrapper-subcommand-protocol(7)`. For more information on
where subcommands are installed see also *FILES AND DIRECTORIES* section of
this manual.


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

-h, \--help
:   Print toolset (*TOOLSET_COMMAND*) help information and exit.  Using `help`
    subcommand without arguments gives the same result.

*SUBCOMMAND*
:   Name of a Command Wrapper or toolset subcommand.  See *SUBCOMMANDS* section
    for list of internal subcommands and some external subcommands that are
    provided as part of standard Command Wrapper installation.

*SUBCOMMAND\_ARGUMENTS*
:   Arguments passed to the *SUBCOMMAND* as they are.  Their meaning depends
    on what specific *SUBCOMMAND* accepts as arguments.  See documentation of a
    specific *SUBCOMMAND*, some of them are listed in *SUBCOMMANDS* section.
    Short help message is provided when following is executed:

    ```
    TOOLSET_COMMAND help SUBCOMMAND
    ```


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

Some of the *EXIT STATUS* codes were inspired by Bash exit codes.  See e.g.
[Advanced Bash-Scripting Guide: Appendix E. Exit Codes With Special Meanings
](http://tldp.org/LDP/abs/html/exitcodes.html) for reference.

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

`126`
:   Failed while trying to execute external subcommand or other external
    command.  Probably a permission problem, or command is not an executable.

`127`
:   Unable to find external subcommand or other external command that we
    Command Wrapper was trying to execute.  Possibly a typo in the command
    name, or an issue with `$PATH` environment variable.

`255`
:   Exit status out of range.  Some subroutine tried to terminate with exit
    code that is out of 0-255 range.

*OTHER*
:   Subcommands are free to use other *EXIT STATUS* codes if there is no
    *EXIT STATUS* defined for that purpose.  See also *SUBCOMMAND PROTOCOL*.


# FILES AND DIRECTORIES

`${XDG_CONFIG_HOME:-$HOME/.config}/command-wrapper/default.dhall`
:   Top-level command wrapper configuration file.  It mostly provides defaults
    that can be overriden by `GLOBAL_OPTIONS`, additional help messages, and
    alias definitions.

    Configuration file itself is described in the *CONFIGURATION FILE* section.

    See also `XDG_CONFIG_HOME` in *ENVIRONMENT VARIABLES* section to better
    understand how the location of the configuration file is determined.

`${XDG_CONFIG_HOME:-$HOME/.config}/${toolset}/default.dhall`
:   Same as already mentioned `.../command-wrapper/default.dhall`.  However
    this one is used by a specific `toolset`, and it is applied on top of the
    `command-wrapper` one.  Configuration for a specific *toolset* is
    `.../command-wrapper/default.dhall` unless it is overriden in
    `.../${toolset}/default.dhall`.

    Configuration file itself is described in the *CONFIGURATION FILE* section.

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
    `${HOME}/.local/lib/command-wrapper/` directory.  Whenever a new *toolset*
    is created, as a symlink to
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

    Command Wrapper treats `NO_COLOR` as a default value.  It can be overridden
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


# CONFIGURATION FILE

Configuration files are read from these locations:

* `${XDG_CONFIG_HOME:-${HOME}/.config}/command-wrapper/default.dhall`
* `${XDG_CONFIG_HOME:-${HOME}/.config}/${toolset}/default.dhall` -- Read only
  if invoked under different name than `command-wrapper`.

Configuration files are then composed together to form one configuration.  The
way the fields are composed depends on the individual fields.  Usually they are
concatenated, or in some case specific toolset configuration overrides
`command-wrapper` configuration.

```
let CommandWrapper =
      https://raw.githubusercontent.com/trskop/command-wrapper/master/dhall/CommandWrapper/Type/package.dhall
      -- Note that adding a hash will allow Dhall to cache the import.
      -- See also `dhall hash --help`.

let commandWrapper =
      https://raw.githubusercontent.com/trskop/command-wrapper/master/dhall/CommandWrapper/package.dhall
      -- Note that adding a hash will allow Dhall to cache the import.
      -- See also `dhall hash --help`.

in
    -- Subcommand aliases.  These can be used to invoke subcommand in
    -- the form:
    --
    --     TOOLSET [GLOBAL_OPTIONS] ALIAS [EXTRA_ARGUMENTS]
    --
    -- Which is then translated into:
    --
    --     TOOLSET [GLOBAL_OPTIONS] COMMAND [ARGUMENTS] [EXTRA_ARGUMENTS]
    --
    -- In addition to the above `help` also understands aliases:
    --
    --     TOOLSET [GLOBAL_OPTIONS] help ALIAS
    --
    -- Is the same as:
    --
    --     TOOLSET [GLOBAL_OPTIONS] help COMMAND
    { aliases
        : List
            -- Name of the alias, i.e. name under which we can execute
            -- this subcommand.
            { alias : Text

            -- Command Wrapper subcommand to be executed under the
            -- name specified in `alias` field.
            , command : Text

            -- Arguments premended to the argument list when invoked.
            , arguments : List Text
            }

    -- Path where to search for subcommands.  Definition from
    --
    --     `${XDG_CONFIG_HOME:-${HOME}/.config}/command-wrapper/default.dhall`
    --
    -- are concatenated with those from
    --
    --     `${XDG_CONFIG_HOME:-${HOME}/.config}/${toolset}/default.dhall`
    , searchPath : List Text

    -- Allows user to override default command wrapper behaviour when
    -- it comes colourised output.  By default Command Wrapper uses
    -- the value:
    --
    --     CommandWrapper.ColourOutput.Auto {=}
    --
    -- Unless `NO_COLOR` environment variable is set, in which case
    -- following is used:
    --
    --     CommandWrapper.ColourOutput.Never {=}
    --
    -- See also `--colour` option and `NO_COLOR` environment variable
    -- descriptions.
    , colourOutput : Optional CommandWrapper.ColourOutput

    -- Extra text to be displayed when `TOOLSET help` or `TOOLSET --help`
    -- is invoked.  It is useful for providing important examples, and
    -- references to additional documentation.
    , extraHelpMessage : Optional Text

    -- Default verbosity to be used when command is invoked.  See
    -- `--verbosity` option for more details.
    , verbosity : CommandWrapper.Verbosity

    } : CommandWrapper.DefaultConfig
```


# SUBCOMMAND PROTOCOL

Command Wrapper follows a specific calling convention for external subcommands.
Subcommands are required to follow a specific protocol when invoked.  This
protocol is described in `command-wrapper-subcommand-protocol(7)` manual page.


# SEE ALSO

command-wrapper-cd(1), command-wrapper-exec(1), command-wrapper-skel(1),
command-wrapper-subcommand-protocol(7)

* [Dhall configuration language](https://dhall-lang.org)
* [XDG Base Directory Specification
  ](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html)
* [Advanced Bash-Scripting Guide: Appendix E. Exit Codes With Special Meanings
  ](http://tldp.org/LDP/abs/html/exitcodes.html)


# BUGS

<https://github.com/trskop/command-wrapper/issues>
