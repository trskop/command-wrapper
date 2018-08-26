# Subcommand Protocol

Protocol that must be respected by all external CommandWrapper subcommands.


## Command Line Arguments

Each subcommand must support following options:

* `--help` which prints subcommand specific help message. This is
  invoked by CommandWrapper's `help` internal subcommand.

**TODO:** Define how help message should look like, especially *Usage* section.
Plan is for it to look something like `TOOLSET SUBCOMMAND ...`.


## Environment Variables

When external subcommand is executed then the following environment variables
are available to it:

* `COMMAND_WRAPPER_EXE` contains full path to CommandWrapper executable, e.g.
  `/home/joe/.local/lib/command-wrapper/command-wrapper`.

* `COMMAND_WRAPPER_NAME` contains name under which CommandWrapper was executed.
  This is not a file path, just command name.

* `COMMAND_WRAPPER_CONFIG` contains a file path to subcommand configuration
  file. Subcommand may ignore it entirely, but if subcommand is using it then
  it has to correctly handle the case when the configuration file doesn't
  exist. It's up to individual subcommand to decide which of these scenarios it
  will use if the configuration doesn't exist:

    1. Use hardcoded defaults. Subcommand may generate the configuration file
       with these defaults. If it does so then the user must be notified by a
       message that it was done so. Such message is subject to verbosity
       setting (see `COMMAND_WRAPPER_VERBOSITY`), i.e. if verbosity is set to
       `silent` then the message is not actually printed.

    2. Fail with error message indicating that the configuration file is
       missing.

* `COMMAND_WRAPPER_VERBOSITY` contains one of:

    * `silent` - Don't print any messages.
    * `normal` - Print only important messages.
    * `verbose` - Print anything that comes in to mind.
    * `annoying` - Print debugging/tracing information.

    Subcommand must respect these values if it's producing any output that is
    not part of user interaction.


## Configuration File

Subcommand may require a configuration file. If it does require one then it
must use the one provided in `COMMAND_WRAPPER_CONFIG`, which is always in Dhall
format. Subcommand must not use any other configuration file, with the notable
exception of it's dependencies, e.g. DNS resolution uses `/etc/resolv.conf`.
However, if it's possible then temporary configuration file should be generated
and passed to the dependency explicitly.

When this is respected by the subcommand then CommandWrapper has full control
over subcommand configuration and command line arguments that are passed to it.
This way it can guarantee consistent UI.

**TODO:** CommandWrapper should provide `dhall` subcommand so that e.g. Bash
subcommands do not have to rely on Dhall tools to be installed separately. This
would also allow tools that want to use JSON format to generate a temporary
file created from the Dhall configuration file.
