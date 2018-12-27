% COMMAND-WRAPPER-SUBCOMMAND-PROTOCOL(7) Subcommand Protocol | v1.0.0
% Peter Trsko
% 22nd of December 2018


# NAME

Protocol that must be respected by all external *Command Wrapper* subcommands.


# DESCRIPTION

TODO


# COMMAND LINE ARGUMENTS

Each subcommand must support following options:

* `--help` which prints subcommand specific help message. This is
  invoked by CommandWrapper's `help` internal subcommand.

* (*PROPOSED*) `--info` which prints program description in Dhall format.

**TODO:** Define how help message should look like, especially *Usage* section.
Plan is for it to look something like `TOOLSET SUBCOMMAND ...`.


# ENVIRONMENT VARIABLES

When external subcommand is executed then the following environment variables
are available to it:

`COMMAND_WRAPPER_EXE`
:   Contains full path to CommandWrapper executable.  Usually
    `${HOME}/.local/lib/command-wrapper/command-wrapper`, e.g.
    `/home/joe/.local/lib/command-wrapper/command-wrapper`.

`COMMAND_WRAPPER_VERSION`
:   Contains version of CommandWrapper executable.

`COMMAND_WRAPPER_NAME`
:   Contains name under which CommandWrapper was executed.  This is not a file
    path, just command name.

    For example if we run:

    ```
    toolset foo
    ```

    Then we get:

    ```
    COMMAND_WRAPPER_NAME=toolset
    ```

`COMMAND_WRAPPER_SUBCOMMAND`
:   Contains name of the subcommand that is being executed from the perspective
    of CommandWrapper.  This is not a file path.

    For example if we run:

    ```
    toolset foo
    ```

    Then we get:

    ```
    COMMAND_WRAPPER_SUBCOMMAND=foo
    ```

`COMMAND_WRAPPER_CONFIG`

:   Contains a file path to subcommand configuration file. Subcommand may it
    entirely, but if subcommand is using it then it has to correctly handle the
    case when the configuration file doesn't exist. It's up to individual
    subcommand to decide which of these scenarios it will use if the
    configuration doesn't exist:

    1. Use hardcoded defaults. Subcommand may generate the configuration file
       with these defaults. If it does so then the user must be notified by a
       message that it was done so. Such message is subject to verbosity
       setting (see `COMMAND_WRAPPER_VERBOSITY`), i.e. if verbosity is set to
       `silent` then the message is not actually printed.

    2. Fail with error message indicating that the configuration file is
       missing.

`COMMAND_WRAPPER_VERBOSITY`
:   Contains one of:

    * `silent` -- Don't print any messages.
    * `normal` -- Print only important messages.
    * `verbose` -- Print anything that comes in to mind.
    * `annoying` -- Print debugging/tracing information.

    Subcommand must respect these values if it's producing any output that is
    not part of user interaction.

`COMMAND_WRAPPER_COLOUR`
:   Contains one of:

    * `always` -- Always use colourised output, even if output is not a
      terminal. This can be useful when, for example, piping output to a pager.
    * `auto` -- Use colourised output when the output is a terminal that
      supports it.
    * `no` -- Never use colourised output.

    Subcommands aren't requred to support colourised output, but if they do
    then they have to respect this environment variable.


# CONFIGURATION FILE

Subcommand may require a configuration file. If it does require one then it
must use the one provided in `COMMAND_WRAPPER_CONFIG`, which is always in
[Dhall](https://github.com/dhall-lang/dhall-lang#readme) format. Subcommand
must not use any other configuration file, with the notable exception of it's
dependencies, e.g. DNS resolution uses `/etc/resolv.conf`.  However, if it's
possible then temporary configuration file should be generated and passed to
the dependency explicitly.

When this is respected by the subcommand then CommandWrapper has full control
over subcommand configuration and command line arguments that are passed to it.
This way it can guarantee consistent UI.

**TODO:** CommandWrapper should provide `dhall` subcommand so that e.g. Bash
subcommands do not have to rely on Dhall tools to be installed separately. This
would also allow tools that want to use JSON format to generate a temporary
file created from the Dhall configuration file.


# SEE ALSO

command-wrapper(1)

[Dhall configuration language](https://dhall-lang.org)