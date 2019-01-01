% COMMAND-WRAPPER-EXEC(1) Command Wrapper 0.1.0 | Command Wrapper
% Peter Trsko
% 23nd December 2018


# NAME

`command-wrapper-exec` -- Execute predefined command with a user specified
environment.


# USAGE

TOOLSET\_COMMAND \[GLOBAL\_OPTIONS] exec {\--ls|-l}

TOOLSET\_COMMAND \[GLOBAL\_OPTIONS] exec COMMAND [\--] \[EXTRA\_COMMAND\_ARGUMENTS]

TOOLSET\_COMMAND \[GLOBAL\_OPTIONS] exec {\--help|-h}

TOOLSET\_COMMAND \[GLOBAL\_OPTIONS] help exec


# DESCRIPTION

This command is similar to shell aliases.  Its configuration defines list of
commands associated with a symbolic name.  For each command we can specify a
working directory, environment variables, and arguments.  Most of the features,
as well as restriction, come from using Dhall for configuration.


# OPTIONS

-l, \--ls
:   List available *COMMAND*s.

COMMAND
:   Command name as it is specified in configuration file.  If available then
    it is executed.  Any *EXTRA_COMMAND_ARGUMENTS* are passed to it.


# EXIT STATUS

For documentation of generic *EXIT STATUS* codes see `command-wrapper(1)`
manual page section *EXIT STATUS*.  Any *EXIT STATUS* codes specific to this
subcommand will be listed below.


# FILES

`${XDG_CONFIG_HOME:-$HOME/.config}/command-wrapper/command-wrapper-exec.dhall`
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
        ${XDG_CONFIG_HOME}/command-wrapper/command-wrapper-exec.dhall
        ```

    * If `XDG_CONFIG_HOME` environment variable is not set then default value
      is used instead:

        ```
        ${HOME}/.config/command-wrapper/command-wrapper-exec.dhall
        ```

    See [XDG Base Directory Specification
    ](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html)
    for more information on rationale behind this.


# EXAMPLES

Lets say
`${XDG_CONFIG_HOME:-$HOME/.config}/command-wrapper/command-wrapper-exec.dhall`
contains the following:

```
let CommandWrapper =
      https://raw.githubusercontent.com/trskop/command-wrapper/master/dhall/CommandWrapper/Type/package.dhall
      sha256:aced9da8f45e34f0e4fd6847c20c42351b71b559691702db89337a8705c28a0e

in  { commands =
        [ { name = "echo"
          , command =
              -- We are ignoring `verbosity` and `colourOutput`, both of those
              -- are passed down from toolset configuration and GLOBAL_OPTIONS.
              -- Therefore, we can construct a command that respects those
              -- options as well.
                λ(verbosity : CommandWrapper.Verbosity)
              → λ(colourOutput : CommandWrapper.ColourOutput)
              → λ(arguments : List Text)
              → { command = "echo"
                , arguments = arguments
                , environment = [] : List CommandWrapper.EnvironmentVariable
                , searchPath = True
                , workingDirectory = None Text
                } : CommandWrapper.ExecCommand
          } : CommandWrapper.ExecNamedCommand
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
      https://raw.githubusercontent.com/trskop/command-wrapper/master/dhall/CommandWrapper/Type/package.dhall
      sha256:aced9da8f45e34f0e4fd6847c20c42351b71b559691702db89337a8705c28a0e

in    λ(cfg : CommandWrapper.DefaultConfig)
    →   cfg
     // { aliases =
              cfg.aliases
            # [ { alias = "hello.world"
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


# SEE ALSO

command-wrapper(1)

* [XDG Base Directory Specification
  ](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html)


# BUGS

<https://github.com/trskop/command-wrapper/issues>
