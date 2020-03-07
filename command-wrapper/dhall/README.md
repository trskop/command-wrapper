# Dhall Libraries

*   Main [CommandWrapper](./CommandWrapper/) library that provides basic types
    for global/toolset configuration, as well as basic definition for commands
    distributed with it.  In the future some of the subcommand-related
    definitions may be put into separate libraries.

*   [Exec](./Exec/) library that provides smart constructor and command line
    completion helpers for building commands using `exec` subcommand.  For more
    information see:

    ```
    TOOLSET help [--man] exec
    ```


# Examples

*   Simple command definitions for [`exec`](./example/exec/) subcommand.


# Dhall Scripts Embedded in Command Wrapper

These Dhall scripts are actually embedded into Command Wrapper during
compilation.

*   [`./completion.dhall`](./completion.dhall)
*   [`./import-direnv-library.dhall`](./import-direnv-library.dhall)
*   [`./import-shell-library.dhall`](./import-shell-library.dhall)
*   [`../../command-wrapper-subcommand/dhall/command-wrapper-style-completion-info.dhall`](../../command-wrapper-subcommand/dhall/command-wrapper-style-completion-info.dhall)
*   [`../../command-wrapper-subcommand/dhall/optparse-completion-info.dhall`](../../command-wrapper-subcommand/dhall/optparse-completion-info.dhall)

Templates of Dhall configuration files for `TOOLSET config --init`:

*   [`./init/command-wrapper/`](./init/command-wrapper/) – Templates of global
    configuration files.
*   [`./init/toolset/`](./init/toolset/) – Templates of toolset-specific
    configuration files.


# Other Dhall Scripts or Templates

*   [`./cached-bash-completion.dhall`](./cached-bash-completion.dhall)
*   [`./toolset-invocation-script.dhall`](./toolset-invocation-script.dhall)
