% COMMAND-WRAPPER(1) Command Wrapper 0.1.0 | Command Wrapper
% Peter Trsko
% 29th February 2020


# NAME

`command-wrapper` -- Command to build Git-style command-line toolsets on top
of.


# USAGE

TOOLSET\_COMMAND \[GLOBAL\_OPTIONS] SUBCOMMAND \[\--] \[SUBCOMMAND\_ARGUMENTS]

TOOLSET\_COMMAND \[GLOBAL\_OPTIONS] help \[SUBCOMMAND]

TOOLSET\_COMMAND \[GLOBAL\_OPTIONS] config \[SUBCOMMAND] \[CONFIG\_OPTIONS]

TOOLSET\_COMMAND \[GLOBAL\_OPTIONS] completion \[COMPLETION\_OPTIONS]

TOOLSET\_COMMAND \[GLOBAL\_OPTIONS] version \[VERSION\_OPTIONS]

TOOLSET\_COMMAND {\--help|-h}

TOOLSET\_COMMAND {\--version|-V}


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
    * *annoying* -- Print debugging/tracing information.

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

\--color=*WHEN*, **\--colour**=*WHEN*
:   Colourise output; *WHEN* can be one of `always` (default if *WHEN* is omitted),
    `auto`, or `never`.  See also `NO_COLOR` in *ENVIRONMENT VARIABLES*
    section.

\--no-color, \--no-colour
:   Same as `--colour=never`.  See also `NO_COLOR` in *ENVIRONMENT VARIABLES*
    section.

\--\[no-]aliases
:   Apply or ignore *SUBCOMMAND* aliases.  This is useful when used from e.g.
    scripts to avoid issues with user defined aliases interfering with how the
    script behaves.

\--change-directory=*DIRECTORY*
:   Change working directory to *DIRECTORY* before doing anything.  Internal and
    external subcommands are always executed after changing working directory.

\--help, -h
:   Print toolset (*TOOLSET_COMMAND*) help information and exit.  Using *help*
    subcommand without arguments gives the same result.  See *help* subcommand
    for more details.

\--version, -V
:   Print version iformation to *stdout* and exit.  Using *version* subcommand
    without arguments gives the same result.  See *version* subcommand for more
    details.

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

*   *help* -- Print help information for top-level toolset command or a
    specified *SUBCOMMAND*.  See `command-wrapper-help(1)` for more information.

*   *config* -- Configuration swiss army knife.  See `command-wrapper-config(1)`
    for more information.

*   *completion* -- Provides shell completion.  See
    `command-wrapper-completion(1)` for more information.

*   *version* -- Print version information to *stdout* and exit.  See
    `command-wrapper-version(1)` for more information.

Some external subcommands are bundled with Command Wrapper itself:

*   *cd* -- Start a new subshell / Tmux window / Kitten terminal emulator
    window / terminal emulator in a selected directory.  See
    `command-wrapper-cd(1)` for more details.

*   *exec* -- Execute predefined command with a user specified environment.
    See `command-wrapper-exec(1)` for more details.

*   *skel* -- Generate subcommand skeleton for specific Command Wrapper
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
        * Unable to change working directory, see `--change-directory` option.

`2`
:   Returned by a subcommand if the Command Wrapper environment wasn't set up
    properly.  In other words *SUBCOMMAND PROTOCOL* was violated.

    This can indicate a version mismatch between Command Wrapper installation,
    and *subcommand*/*toolset* installation.

`125`
:   Functionality or an internal subcommand is not yet implemented.

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

`${COMMAND_WRAPPER_USER_CONFIG_DIR:-${XDG_CONFIG_HOME:-$HOME/.config}}/command-wrapper/default.dhall`
:   User-level command wrapper configuration file.  It mostly provides defaults
    that can be overriden by `GLOBAL_OPTIONS`, additional help messages, and
    alias definitions.

    Configuration file itself is described in the *CONFIGURATION FILE* section.

    See also `COMMAND_WRAPPER_USER_CONFIG_DIR` and `XDG_CONFIG_HOME` in
    *ENVIRONMENT VARIABLES* section to better understand how the location of
    the configuration file is determined.

`${COMMAND_WRAPPER_LOCAL_CONFIG_DIR}/command-wrapper/default.dhall`
:   If `COMMAND_WRAPPER_LOCAL_CONFIG_DIR` is defined and the
    `${COMMAND_WRAPPER_LOCAL_CONFIG_DIR}/command-wrapper/default.dhall` file
    exists then it is used in addition to
    `${COMMAND_WRAPPER_USER_CONFIG_DIR:-${XDG_CONFIG_HOME:-$HOME/.config}}/command-wrapper/default.dhall`
    as a more specific configuration file.

    Configuration file and how configuration files are composed is described in
    the *CONFIGURATION FILE* section.

    See also `COMMAND_WRAPPER_LOCAL_CONFIG_DIR` in *ENVIRONMENT VARIABLES*
    section to better understand how the location of the configuration file is
    determined.

`${COMMAND_WRAPPER_USER_CONFIG_DIR:-${XDG_CONFIG_HOME:-$HOME/.config}}/${toolset}/default.dhall`
:   Same as already mentioned `.../command-wrapper/default.dhall`.  However
    this one is used by a specific `toolset`, and it is applied on top of the
    `command-wrapper` one.  Configuration for a specific *toolset* is
    `.../command-wrapper/default.dhall` unless it is overriden in
    `.../${toolset}/default.dhall`.

    Configuration file and how configuration files are composed is described in
    the *CONFIGURATION FILE* section.

    See also `COMMAND_WRAPPER_USER_CONFIG_DIR` `XDG_CONFIG_HOME` in
    *ENVIRONMENT VARIABLES* section to better understand how the location of
    the configuration file is determined.

`${COMMAND_WRAPPER_LOCAL_CONFIG_DIR}/${toolset}/default.dhall`
:   If `COMMAND_WRAPPER_LOCAL_CONFIG_DIR` is defined and the
    `${COMMAND_WRAPPER_LOCAL_CONFIG_DIR}/${toolset}/default.dhall` file
    exists then it is used in addition to
    `${COMMAND_WRAPPER_USER_CONFIG_DIR:-${XDG_CONFIG_HOME:-$HOME/.config}}/${toolset}/default.dhall`
    as a more specific configuration file.

    Configuration file and how configuration files are composed is described in
    the *CONFIGURATION FILE* section.

    See also `COMMAND_WRAPPER_LOCAL_CONFIG_DIR` in *ENVIRONMENT VARIABLES*
    section to better understand how the location of the configuration file is
    determined.

`${COMMAND_WRAPPER_USER_CONFIG_DIR:-${XDG_CONFIG_HOME:-$HOME/.config}}/${toolset}/${toolset}-${subcommand}.dhall`
:   Subcommand specific configuration file.  There is not much we can say about
    them, since every subcommand can have its own definition.

    See also:

    *   `command-wrapper-subcommand-protocol(7)` for more details on how
        subcommands use their configuration files.
    *   `COMMAND_WRAPPER_USER_CONFIG_DIR` and `XDG_CONFIG_HOME` in
        *ENVIRONMENT VARIABLES* section to better understand
        how the location of the configuration file is determined.

`${COMMAND_WRAPPER_LOCAL_CONFIG_DIR}/${toolset}/${toolset}-${subcommand}.dhall`
:   Subcommand specific configuration file. Same as
    `${COMMAND_WRAPPER_USER_CONFIG_DIR:-${XDG_CONFIG_HOME:-$HOME/.config}}/${toolset}/${toolset}-${subcommand}.dhall`,
    but it has higher priority. In other words
    `${COMMAND_WRAPPER_LOCAL_CONFIG_DIR}/${toolset}/${toolset}-${subcommand}.dhall`
    is used when it exists instead of
    `${COMMAND_WRAPPER_USER_CONFIG_DIR:-${XDG_CONFIG_HOME:-$HOME/.config}}/${toolset}/${toolset}-${subcommand}.dhall`.

    See also:

    *   `command-wrapper-subcommand-protocol(7)` for more details on how
        subcommands use their configuration files.
    *   `COMMAND_WRAPPER_LOCAL_CONFIG_DIR` in *ENVIRONMENT VARIABLES* section to
        better understand how the location of the configuration file is
        determined.

`${COMMAND_WRAPPER_USER_CONFIG_DIR:-${XDG_CONFIG_HOME:-$HOME/.config}}/${toolset}/command-wrapper-${subcommand}.dhall`
:   Subcommand specific configuration file for a *subcommand* that is part of the
    Command Wrapper distribution.  This allows each *toolset* to provide
    its own configuration for these subcommands.

    If global configuration should be reused by the *toolset* then following
    pattern should be employed.  Example is for `exec` subcommand, but it can
    be used for any subcommand:

    ```
    -- This file has following file path:
    -- ${COMMAND_WRAPPER_USER_CONFIG_DIR:-${XDG_CONFIG_HOME:-$HOME/.config}}/${toolset}/command-wrapper-exec.dhall

    let global = ../command-wrapper/command-wrapper-exec.dhall

    in    global
        //  { commands =
                  ./exec/commands.dhall
                # global.commands
            }
    ```

    Preserving `command-wrapper-` prefix in `command-wrapper-${subcommand}.dhall`
    configuration file name makes it explicit from where the subcommand came
    from.  Toolsets may define subcommands with the same name, but the prefix
    will be different.

`${COMMAND_WRAPPER_LOCAL_CONFIG_DIR}/${toolset}/command-wrapper-${subcommand}.dhall`
:   Subcommand specific configuration file. Same as
    `${COMMAND_WRAPPER_USER_CONFIG_DIR:-${XDG_CONFIG_HOME:-$HOME/.config}}/${toolset}/command-wrapper-${subcommand}.dhall`,
    but it has higher priority. In other words
    `${COMMAND_WRAPPER_LOCAL_CONFIG_DIR}/${toolset}/command-wrapper-${subcommand}.dhall`
    is used when it exists instead of
    `${COMMAND_WRAPPER_USER_CONFIG_DIR:-${XDG_CONFIG_HOME:-$HOME/.config}}/${toolset}/command-wrapper-${subcommand}.dhall`.

`${XDG_DATA_HOME:-$HOME/.local/share}/man/`
:   Command Wrapper manual pages are installed into this directory so that
    standard `man` command is able to find them.

`${HOME}/.local/lib/command-wrapper/command-wrapper`
:   Default installation path for `command-wrapper` executable.  It's not
    expected to be in `$PATH`, hence the use of libexec-style directory.
    See also description of `${HOME}/.local/lib/command-wrapper/`.

`${HOME}/.local/lib/command-wrapper/command-wrapper-${subcommand}`
:   Default installation path for a common/global *subcommand* executable.
    It's not expected to be in `$PATH`, hence the use of libexec-style
    directory.  See also description of `${HOME}/.local/lib/command-wrapper/`.

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


# STANDARD DIRECTORY LAYOUT

Location of configuration can be overridden using `$XDG_CONFIG_HOME`
environment variable.  See *FILES AND DIRECTORIES*, *ENVIRONMENT VARIABLES*, and
*CONFIGURATION FILE* sections for more details on how to override certain
locations.

```
~/
├── .config/
│   ├── ...
│   ├── command-wrapper/
│   │   ├── default.dhall
│   │   ├── command-wrapper-cd.dhall
│   │   ├── command-wrapper-exec.dhall
│   │   ├── command-wrapper-skel.dhall
│   │   └── ...
│   └── ${toolset}/
│       ├── default.dhall
│       ├── command-wrapper-cd.dhall
│       ├── command-wrapper-exec.dhall
│       ├── command-wrapper-skel.dhall
│       ├── ${toolset}-${toolsetSubcommand0}.dhall
│       ├── ...
│       └── ${toolset}-${toolsetSubcommandN}.dhall
│
├── .local/
│   ├── ...
│   ├── lib/
│   │   ├── ...
│   │   ├── command-wrapper/
│   │   │   ├── command-wrapper
│   │   │   ├── command-wrapper-cd
│   │   │   ├── command-wrapper-exec
│   │   │   ├── command-wrapper-skel
│   │   │   └── ...
│   │   └── ${toolset}/
│   │       ├── ${toolset}-${toolsetSubcommand0}
│   │       ├── ...
│   │       └── ${toolset}-${toolsetSubcommandN}
│   │
│   └── share/
│       ├── man/
│       │   ├── man1
│       │   │   ├── command-wrapper.1.gz
│       │   │   ├── command-wrapper-cd.1.gz
│       │   │   ├── command-wrapper-completion.1.gz
│       │   │   ├── command-wrapper-config.1.gz
│       │   │   ├── command-wrapper-exec.1.gz
│       │   │   ├── command-wrapper-help.1.gz
│       │   │   ├── command-wrapper-skel.1.gz
│       │   │   ├── command-wrapper-version.1.gz
│       │   │   └── ...
│       │   ├── man7/
│       │   │   ├── command-wrapper-subcommand-protocol.7.gz
│       │   │   └── ...
│       │   └── ...
│       └── ...
│
└── bin/
    ├── ${toolset} --> ../.local/lib/command-wrapper/command-wrapper
    └── ...
```

Interestingly `man` is able to find manual pages in `$HOME/.local/share/man` if
`$HOME/.local/bin` is in `$PATH`.  This was tested only on systems with
<http://man-db.nongnu.org/> installed.  To test if `$HOME/.local/share/man` is
used by `man` run `manpath` command, which should print out something like:

```
/home/user/.local/share/man:/usr/local/man:/usr/local/share/man:/usr/share/man
```


# ENVIRONMENT VARIABLES

`COMMAND_WRAPPER_USER_CONFIG_DIR`, `XDG_CONFIG_HOME`
:   Overrides where Command Wrapper looks for user configuration files.

    Following logic is used when looking up *toolset* configuration files:

    *   If `COMMAND_WRAPPER_USER_CONFIG_DIR` environment variable is set then
        the configuration file has path:

        ```
        ${COMMAND_WRAPPER_USER_CONFIG_DIR}/${toolset}/default.dhall
        ```

    *   If `COMMAND_WRAPPER_USER_CONFIG_DIR` environment variable is not set,
        but `XDG_CONFIG_HOME` environment variable is set then the
        configuration file has path:

        ```
        ${XDG_CONFIG_HOME}/${toolset}/default.dhall
        ```

    *   If `XDG_CONFIG_HOME` environment variable is not set then default value
        is used instead:

        ```
        ${HOME}/.config/${toolset}/default.dhall
        ```

    For subcommand configuration files it is:

    *   If `COMMAND_WRAPPER_USER_CONFIG_DIR` environment variable is set then
        the configuration file has path:

        ```
        ${COMMAND_WRAPPER_USER_CONFIG_DIR}/${toolset}/${toolset}-${subcommand}.dhall
        ```

    *   If `COMMAND_WRAPPER_USER_CONFIG_DIR` environment variable is not set,
        but `XDG_CONFIG_HOME` environment variable is set then the
        configuration file has path:

        ```
        ${XDG_CONFIG_HOME}/${toolset}/${toolset}-${subcommand}.dhall
        ```

    *   If `XDG_CONFIG_HOME` environment variable is not set then default value
        is used instead:

        ```
        ${HOME}/.config/${toolset}/${toolset}-${subcommand}.dhall
        ```

    Reason for having `COMMAND_WRAPPER_USER_CONFIG_DIR` is that
    `XDG_CONFIG_HOME` is widely used standard.  Overriding its value may have
    unforseen consequences if we only want to change that directory only for
    Command Wrapper itself, but not other tools.  This is especially important
    for functionality/subcommands that call other tools.

    See [XDG Base Directory Specification
    ](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html)
    for more information on rationale behind this.

    This variable is kept in the environment when external commands and
    external subcommands are executed.

`COMMAND_WRAPPER_LOCAL_CONFIG_DIR`
:   If this environment variable is set it introduces another level of
    configuration.

    User configuration file for toolset (see documentation for
    `COMMAND_WRAPPER_USER_CONFIG_DIR` and `XDG_CONFIG_HOME` environment
    variables) is still handled the same way and read, but following
    configuration file has precedence over it:

    ```
    ${COMMAND_WRAPPER_LOCAL_CONFIG_DIR}/${toolset}/default.dhall
    ```

    See section *CONFIGURATION FILE* for more detailed description of how
    configuration files are combined.

    Having the ability to specify local, or project-level, configuration in
    addition to user-level can be very useful when using tools like [`direnv`
    ](https://direnv.net/).

    This variable is kept in the environment when external commands and
    external subcommands are executed.

`COMMAND_WRAPPER_SYSTEM_CONFIG_DIR`
:   When Command Wrapper binary is compiled statically then it responds to this

    variable being set by introducing yet another level in configuration
    hierarchy.  Toolset configuration files in provided directory are read
    before the ones in `COMMAND_WRAPPER_USER_CONFIG_DIR`/`XDG_CONFIG_HOME`.
    Subcommand configuration files are tried only if they are not found in
    `COMMAND_WRAPPER_LOCAL_CONFIG_DIR`, `COMMAND_WRAPPER_USER_CONFIG_DIR`, and
    `XDG_CONFIG_HOME`, in that order.

    This variable is kept in the environment when external commands and
    external subcommands are executed.

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
       COMMAND_WRAPPER_INVOKE_AS="${COMMAND_WRAPPER_NAME}" "${COMMAND_WRAPPER_EXE}" "$@"
    }
    ```

    This environment variable is unset before external command or external
    subcommand is executed.  If external command/subcommand is calling
    different toolset then this would interfere.  External subcommands will
    still have access to this value via appropriate environment variable from
    Subcommand Protocol.  See also `command-wrapper-subcommand-protocol(7)` for
    more details on how subcommands are invoked.

`COMMAND_WRAPPER_FACADE`
:   Command Wrapper needs to know absolute file path to its underlying
    executable.  This is so that subcommands can be given a reliable way how
    to call Command Wrapper.  Setting this variable overrides default
    executable resolution and provided value is used instead.

    Very useful in Nix derivations (Nix package definitions) when creating
    wrapper scripts to pass Nix-specific environment.  See following links for
    more information:

    *   [Nixpkgs Users and Contributors Guide: 6.6 Shell functions: makeWrapper](https://nixos.org/nixpkgs/manual/#fun-makeWrapper)
    *   [Nixpkgs Users and Contributors Guide: 6.6 Shell functions: wrapProgram](https://nixos.org/nixpkgs/manual/#fun-wrapProgram)

    Environment variable is unset before external subcommand is executed.

    This environment variable is unset before external command or external
    subcommand is executed.  Having is present can interfere in what underlying
    command intended to execute.  External subcommands still have access to
    this value via appropriate environment variable from Subcommand Protocol.
    See also `command-wrapper-subcommand-protocol(7)` for more details on how
    subcommands are invoked.

`COMMAND_WRAPPER_PATH`
:   Default search path for external subcommands.  Any value that is specified
    in configuration file (see *CONFIGURATION FILE* section for details) is
    appended to this value.

    Here is an example shell session (Bash) that demonstrates how this works:

    ```
    $ mkdir subcommands
    $ cat > subcommands/command-wrapper-test <<"EOF"
    #!/usr/bin/env bash

    echo "Test World!"
    EOF
    $ chmod +x subcommands/command-wrapper-test
    $ COMMAND_WRAPPER_PATH="${PWD}/subcommands" ~/.local/lib/command-wrapper/command-wrapper test
    Test World!
    ```

    This variable is kept in the environment when external commands and
    external subcommands are executed.

`COMMAND_WRAPPER_MANPATH`
:   Default search path for manual pages.  Any value that is specified
    in configuration file (see *CONFIGURATION FILE* section for details) is
    appended to this value.

    This variable is kept in the environment when external commands and
    external subcommands are executed.

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

    Value of this environment variable is kept when external subcommand is
    executed, however, external subcommands should use Subcommand Protocol to
    figure out if they should use colours in their output or not.


# CONFIGURATION FILE

Toolset configuration files are read from these locations (in specified order):

* `${COMMAND_WRAPPER_USER_CONFIG_DIR:-${XDG_CONFIG_HOME:-${HOME}/.config}}/command-wrapper/default.dhall`
* `${COMMAND_WRAPPER_LOCAL_CONFIG_DIR}/command-wrapper/default.dhall`
* `${COMMAND_WRAPPER_USER_CONFIG_DIR:-${XDG_CONFIG_HOME:-${HOME}/.config}}/${toolset}/default.dhall`
  -- Read only if invoked under different name than `command-wrapper`.
* `${COMMAND_WRAPPER_LOCAL_CONFIG_DIR}/${toolset}/default.dhall`
  -- Read only if invoked under different name than `command-wrapper`.

Configuration files are then composed together to form one configuration.  The
way the fields are composed depends on the individual fields.  Usually they are
concatenated, or in some case specific toolset configuration overrides
`command-wrapper` configuration, i.e. configuration files mentioned later in
the above list override those that are specified before it.

Toolset configuration file has following type:

```
let CommandWrapper =
      https://raw.githubusercontent.com/trskop/command-wrapper/master/command-wrapper/dhall/CommandWrapper/package.dhall
      -- Note that adding a hash will allow Dhall to cache the import.
      -- See also `dhall hash --help`.

in  -- Subcommand aliases.  These can be used to invoke subcommand in
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

            -- Optional alias description which serves as a short help message.
            , description : Optional Text

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

    -- Path where to search for manual pages.  Definition from
    --
    --     `${XDG_CONFIG_HOME:-${HOME}/.config}/command-wrapper/default.dhall`
    --
    -- are concatenated with those from
    --
    --     `${XDG_CONFIG_HOME:-${HOME}/.config}/${toolset}/default.dhall`
    , manPath : List Text

    -- Description of the toolset command printed as part of help message.
    , description : Optional Text

    -- Allows user to override default command wrapper behaviour when
    -- it comes colourised output.  By default Command Wrapper uses
    -- the value:
    --
    --     CommandWrapper.ColourOutput.Type.Auto
    --
    -- Unless `NO_COLOR` environment variable is set, in which case
    -- following is used:
    --
    --     CommandWrapper.ColourOutput.Type.Never
    --
    -- See also `--colour` option and `NO_COLOR` environment variable
    -- descriptions.
    , colourOutput : Optional CommandWrapper.ColourOutput.Type

    -- Extra text to be displayed when `TOOLSET help` or `TOOLSET --help`
    -- is invoked.  It is useful for providing important examples, and
    -- references to additional documentation.
    , extraHelpMessage : Optional Text

    -- Default verbosity to be used when command is invoked.  See
    -- `--verbosity` option for more details.
    , verbosity : Optional CommandWrapper.Verbosity.Type
    }
```

Imports in the above description aren't necessary.  To get fully
normalised/desugared version just paste it as an input to `dhall` command.

It is highly advised  to use imports and to use smart constructor syntax
instead of listing all values.  It makes it easier to migrate configuration
between versions.  Smart constructor syntax example:

```Dhall
let CommandWrapper =
      https://raw.githubusercontent.com/trskop/command-wrapper/master/command-wrapper/dhall/CommandWrapper/package.dhall
      -- Note that adding a hash will allow Dhall to cache the import.
      -- See also `dhall hash --help`.

in  CommandWrapper.ToolsetConfig::{
    , aliases = ./aliases-common.dhall # ./exec-aliases.dhall

    , searchPath =
        CommandWrapper.ToolsetConfig.defaultSearchPath
        env:HOME as Text
        "command-wrapper"
    }
```


# SUBCOMMAND PROTOCOL

Command Wrapper follows a specific calling convention for external subcommands.
Subcommands are required to follow a specific protocol when invoked.  This
protocol is described in `command-wrapper-subcommand-protocol(7)` manual page.


# SEE ALSO

command-wrapper-cd(1), command-wrapper-completion(1), command-wrapper-config(1),
command-wrapper-exec(1), command-wrapper-help(1), command-wrapper-skel(1),
command-wrapper-version(1), command-wrapper-subcommand-protocol(7),
command-wrapper-bash-library(7)

* [Dhall configuration language](https://dhall-lang.org)
* [XDG Base Directory Specification
  ](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html)
* [Advanced Bash-Scripting Guide: Appendix E. Exit Codes With Special Meanings
  ](http://tldp.org/LDP/abs/html/exitcodes.html)
 [`direnv` -- unclutter your `.profile`](https://direnv.net/)


# BUGS

<https://github.com/trskop/command-wrapper/issues>
