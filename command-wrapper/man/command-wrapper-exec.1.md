% COMMAND-WRAPPER-EXEC(1) Command Wrapper 0.1.0 | Command Wrapper
% Peter Trsko
% 25th October 2019


# NAME

`command-wrapper-exec` -- Execute predefined command with a user specified
environment.


# USAGE

TOOLSET\_COMMAND \[GLOBAL\_OPTIONS] exec \[\--notify] [\--] *COMMAND*
\[*COMMAND\_ARGUMENTS*]

TOOLSET\_COMMAND \[GLOBAL\_OPTIONS] exec {\--list|\--ls|-l|\--tree|-t}

TOOLSET\_COMMAND \[GLOBAL\_OPTIONS] exec \--expression=*EXPRESSION* \[\--notify]
[\--] \[*COMMAND\_ARGUMENTS*]

TOOLSET\_COMMAND \[GLOBAL\_OPTIONS] exec \--print [\--] *COMMAND*
\[*COMMAND\_ARGUMENTS*]

TOOLSET\_COMMAND \[GLOBAL\_OPTIONS] exec \--print-completion \--index=*NUM*
\--shell=*SHELL* *COMMAND* [\--] \[*COMMAND\_ARGUMENTS*]

TOOLSET\_COMMAND \[GLOBAL\_OPTIONS] exec {\--help|-h}

TOOLSET\_COMMAND \[GLOBAL\_OPTIONS] help exec


# DESCRIPTION

This command is similar to shell aliases.  Its configuration defines list of
commands (in form of a Dhall function) associated with a symbolic name.  For
each command we can specify a working directory, environment variables, and
arguments.

Most of the features, as well as restriction, come from using Dhall for
configuration.  Biggest advantage is probably that it is possible to share
command definitions in the form of Dhall files that we can safely import, even
from a URL.  All of this can be done without dropping to general purpose
scripting language like Bash.


# OPTIONS

\--list, \--ls, -l
:   List available *COMMAND*s.

\--tree, \-t
:   List available *COMMAND*s in tree-like form treating dots (`.`) as
    separators.

    Let's say that we have following commands reported by `--list`:

    ```
    build.back-end
    build.back-end.locally
    build.back-end.remotely
    build.front-end
    build.back-end.locally
    build.back-end.remotely
    debug
    echo
    ```

    These would be displayed as a following tree:

    ```
    ├── build
    │   ├── back-end*
    │   │   ├── locally*
    │   │   └── remotely*
    │   └── front-end*
    │       ├── locally*
    │       └── remotely*
    ├── debug*
    └── echo*
    ```

    The star (`*`) character indicates which commands are executable, i.e. can
    be called as:

    ```
    TOOLSET_COMMAND exec COMMAND [--] [COMMAND_ARGUMENTS]
    ```

\--expression=*EXPRESSION*
:   Execute Dhall *EXPRESSION*.  The *EXPRESSION* has to have following type:

    ```
    let CommandWrapper =
          https://raw.githubusercontent.com/trskop/command-wrapper/master/dhall/CommandWrapper/package.dhall

    in    ∀(verbosity : CommandWrapper.Verbosity.Type)
        → ∀(colour : CommandWrapper.ColourOutput.Type)
        → ∀(arguments : List Text)
        → CommandWrapper.ExecCommand.Type
    ```

    For example:

    ```
    let CommandWrapper =
          https://raw.githubusercontent.com/trskop/command-wrapper/master/dhall/CommandWrapper/package.dhall

    in    λ(verbosity : CommandWrapper.Verbosity.Type)
        → λ(colourOutput : CommandWrapper.ColourOutput.Type)
        → λ(arguments : List Text)
        → { arguments = arguments
          , command = "echo"
          , environment = [] : List CommandWrapper.EnvironmentVariable.Type
          , searchPath = True
          , workingDirectory = None Text
          }
    ```

    This is very useful when designing new commands.

\--notify
:   Send desktop notification when the command is done.

\--print
:   Print command as it will be executed in Dhall format.  Can be used as
    dry-run functionality, for debugging, and for creating template when adding
    new command.  Any *COMMAND_ARGUMENTS* are passed to the Dhall
    function before it being evaluated.

    Let's say that configuration contains following command definition:

    ```
    let CommandWrapper =
          https://raw.githubusercontent.com/trskop/command-wrapper/master/dhall/CommandWrapper/package.dhall

    in  { commands =
            [   { name = "echo"
                , description = Some "Call echo command (not the shell builtin)."
                , command =
                      λ(_ : CommandWrapper.Verbosity.Type)
                    → λ(_ : CommandWrapper.ColourOutput.Type)
                    → λ(arguments : List Text)
                    →   { command = "echo"
                        , arguments = arguments
                        , environment =
                            [] : List CommandWrapper.EnvironmentVariable.Type
                        , searchPath = True
                        , workingDirectory = None Text
                        }
                      : CommandWrapper.ExecCommand.Type
                , completion =
                    None
                      (   CommandWrapper.Shell.Type
                        → Natural
                        → List Text
                        → CommandWrapper.ExecCommand.Type
                      )
                , notifyWhen = None CommandWrapper.NotifyWhen.Type
                }
              : CommandWrapper.ExecNamedCommand.Type
            ]
        }
    ```

    If we call following:

    ```
    TOOLSET_COMMAND exec --print echo foo bar
    ```

    Then we'll get output like this one:

    ```
    { command = "echo"
    , arguments = [ "foo", "bar" ]
    , environment = [] : List { name : Text, value : Text }
    , searchPath = True
    , workingDirectory = None Text
    }
    ```

    See also *CONFIGURATION FILE* section for more information.

\--print-completion
:   Similar to `--print`, but prints command that would be used to do command
    line completion if it was invoked.  Additional options `--index=`*NUM*, and
    `--shell=`*SHELL* are passed to the command line completion command.

\--help, -h
:   Display help information and exit.  Same as `TOOLSET_COMMAND help exec`.

*COMMAND*
:   `COMMAND` is a symbolic command name as it is specified in configuration
    file.  If available then it is executed.  Any *COMMAND_ARGUMENTS* are
    passed to it.  See *CONFIGURATION FILE* section for more details.

*COMMAND_ARGUMENTS*
:   Extra arguments that are passed to the command referenced by *COMMAND*.  See
    *CONFIGURATION FILE* section for more details.


# EXIT STATUS

For documentation of generic *EXIT STATUS* codes see `command-wrapper(1)`
manual page section *EXIT STATUS*.  Any *EXIT STATUS* codes specific to this
subcommand will be listed below.


# FILES

`${XDG_CONFIG_HOME:-$HOME/.config}/${toolset}/command-wrapper-exec.dhall`
:   Configuration file specifies *COMMAND*s that can be invoked.  Command
    configuration is actually a function with a name associated with it.

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
        ${XDG_CONFIG_HOME}/${toolset}/command-wrapper-exec.dhall
        ```

    * If `XDG_CONFIG_HOME` environment variable is not set then default value
      is used instead:

        ```
        ${HOME}/.config/${toolset}/command-wrapper-exec.dhall
        ```

    See [XDG Base Directory Specification
    ](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html)
    for more information on rationale behind this.


# CONFIGURATION FILE

See *FILES* section for documentation on where `exec`'s configuration file is
located.

Configuration file of `exec` subcommand has following type signature:

```
let CommandWrapper =
      https://raw.githubusercontent.com/trskop/command-wrapper/master/dhall/CommandWrapper/package.dhall
      -- Note that adding a hash will allow Dhall to cache the import.
      -- See also `dhall hash --help`.

in  -- List of commands that `exec` subcommand can execute.
    { commands : List CommandWrapper.ExecNamedCommand.Type
    }
```

If we expand `CommandWrapper.ExecNamedCommand.Type` type we get:

```
let CommandWrapper =
      https://raw.githubusercontent.com/trskop/command-wrapper/master/dhall/CommandWrapper/package.dhall
      -- Note that adding a hash will allow Dhall to cache the import.
      -- See also `dhall hash --help`.

in  -- List of commands that `exec` subcommand can execute.
    { commands :
        List
          -- Symbolic name for this command (`COMMAND`)
          -- that can be used to invoke it via:
          --
          --     TOOLSET_COMMAND exec COMMAND
          --
          -- See `COMMAND` description for more details.
          { name : Text

          -- Description of `COMMAND` which is printed when `exec`
          -- is invoked with `--list` option.
          , description : Optional Text

          -- Function that constructs command description
          -- that we can execute.
          , command :
              -- Verbosity is taken from following
              -- environment variable:
              --
              --     COMMAND_WRAPPER_VERBOSITY
              --
              -- See `command-wrapper-subcommand-protocol(7)`
              -- for more details on its purpose.
                ∀(verbosity : CommandWrapper.Verbosity.Type)

              -- Colour output preferences are taken from
              -- following environment variable:
              --
              --     COMMAND_WRAPPER_COLOUR
              --
              -- See `command-wrapper-subcommand-protocol(7)`
              -- for more details on its purpose.
              → ∀(colourOutput : CommandWrapper.ColourOutput.Type)

              -- Extra arguments passed on command line.
              -- See `EXTRA_COMMAND_ARGUMENTS` for more
              -- details.
              → ∀(arguments : List Text)

                -- Either command name of fill file path
                -- to an executable.  We search for it in
                -- `$PATH` only if `searchPath = True`
              → { command : Text

                -- Arguments as they are passed to `command`
                -- when executed.  Usually we want to append
                -- `EXTRA_COMMAND_ARGUMENTS`, but not always.
                , arguments : List Text

                -- Look for `command` in `$PATH`?
                --
                -- * True - Yes, search `$PATH` for `command`.
                -- * False - No, ignore `$PATH` and execute
                --   `command` as it is.  Usually requires
                --   `command` to be a full path.
                , searchPath : Bool

                -- Additional environment variables to
                -- pass to the `command` when executed.
                -- They may override existing environment
                -- variables.
                , environment : List CommandWrapper.EnvironmentVariable.Type

                -- Change working directory before executing
                -- `command`, if specified.  Otherwise keep
                -- working directory unchanged.
                , workingDirectory : Optional Text
                }

          -- Alternative command to invoke when performing
          -- command line completion.
          , completion :
              Optional
              (   ∀(shell : CommandWrapper.Shell.Type)
                → ∀(index : Natural)
                → ∀(arguments : List Text)
                → CommandWrapper.ExecCommand.Type
              )

          , notifyWhen : Optional CommandWrapper.NotifyWhen.Type
          }
    }
```

See also following (*EXAMPLES*) section.


# EXAMPLES

Lets say
`${XDG_CONFIG_HOME:-$HOME/.config}/command-wrapper/command-wrapper-exec.dhall`
contains the following:

```
let CommandWrapper =
      https://raw.githubusercontent.com/trskop/command-wrapper/master/dhall/CommandWrapper/package.dhall
      -- Note that adding a hash will allow Dhall to cache the import.
      -- See also `dhall hash --help`.

in  { commands =
        -- Use smart constructor to create named commands to avoid upgrade
        -- issues any time there is a change in definition of
        -- `CommandWrapper.ExecNamedCommand.Type`.
        [   CommandWrapper.ExecNamedCommand.namedCommand
            -- Name of the command we are defining, this will be what needs to
            -- be passed to `exec` to invoke it:
            -- ```
            -- TOOLSET_COMMAND [GLOBAL_OPTIONS] exec [EXEC_OPTIONS] echo [ARGUMENTS]
            -- ```
            "echo"
            -- We are ignoring `verbosity` and `colourOutput`, both of those
            -- are passed down from toolset configuration and GLOBAL_OPTIONS.
            -- Therefore, we can construct a command that respects those
            -- options as well.
            (   λ(verbosity : CommandWrapper.Verbosity.Type)
              → λ(colourOutput : CommandWrapper.ColourOutput.Type)
              → λ(arguments : List Text)
              →   { command =
                      "echo"
                  , arguments =
                      arguments
                  , environment =
                      CommandWrapper.Command.emptyEnvironment
                  , searchPath =
                      True
                  , workingDirectory =
                      None Text
                  }
                : CommandWrapper.ExecCommand.Type
            )
          //  { description = Some "Call echo command (not the shell builtin)."
              }
        ]
    }
```

Then we can run it as:

```
user@machine ~ $ TOOLSET_COMMAND exec echo hello world
hello world
```

We can simplify that if we modify `TOOLSET_COMMAND` configuration to provide an
alias for it.  Following is a function that if applied to toolset configuration
will add an alias named `hello.world`:

```
let CommandWrapper =
      https://raw.githubusercontent.com/trskop/command-wrapper/master/dhall/CommandWrapper/package.dhall
      -- Note that adding a hash will allow Dhall to cache the import.
      -- See also `dhall hash --help`.

in    λ(cfg : CommandWrapper.ToolsetConfig.Type)
    →   cfg
     // { aliases =
              cfg.aliases
            # [ { alias = "hello.world"
                , description = None Text
                , command = "exec"
                , arguments = ["echo", "hello", "world"] : List Text
                }
              ]
        }
```

After that we can run:

```
user@machine ~ $ TOOLSET_COMMAND hello.world
hello world
user@machine ~ $ TOOLSET_COMMAND hello.world again
hello world again
```

Following Dhall expression will create aliases for all exec commands:

```
let CommandWrapper =
      https://raw.githubusercontent.com/trskop/command-wrapper/master/dhall/CommandWrapper/package.dhall

let execConfig = ../command-wrapper-exec.dhall

in  CommandWrapper.ExecNamedCommand.namedCommandsToAliases execConfig.commands
```

It is ment to be put into
`${XDG_CONFIG_HOME:-$HOME/.config}/command-wrapper/default/exec-aliases.dhall`
and included in
`${XDG_CONFIG_HOME:-$HOME/.config}/command-wrapper/default.dhall`.


# SEE ALSO

command-wrapper(1)

* [XDG Base Directory Specification
  ](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html)


# BUGS

<https://github.com/trskop/command-wrapper/issues>