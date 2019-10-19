# Dhall Libraries

* Main [CommandWrapper](./CommandWrapper/) library that provides basic types
  for global/toolset configuration, as well as basic definition for commands
  distributed with it.  In the future some of the subcommand-related
  definitions may be put into separate libraries.

* [Exec](./CommandWrapper/) library that provides smart constructor and command
  line completion helpers for building commands using `exec` subcommand.  For
  more information see:

  ```
  TOOLSET help [--man] exec
  ```

# Examples

* Simple command definitions for [`exec`](./example/exec/) subcommand.


# Dhall Scripts Embedded in CommandWrapper

These Dhall scripts are actually embedded into CommandWrapper during
compilation.

* [`./command-wrapper-style-completion-info.dhall`
  ](./command-wrapper-style-completion-info.dhall)
* [`./completion.dhall`](./completion.dhall)
* [`./optparse-completion-info.dhall`](./optparse-completion-info.dhall)
