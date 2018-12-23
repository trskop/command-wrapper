# Command Wrapper

[![Haskell Programming Language](https://img.shields.io/badge/language-Haskell-blue.svg)][Haskell.org]
[![BSD3 License](http://img.shields.io/badge/license-BSD3-brightgreen.svg)][tl;dr Legal: BSD3]


## Description

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


## Internal Subcommands

```
Usage:

    TOOLSET_COMMAND [GLOBAL_OPTIONS] help [HELP_OPTIONS] [SUBCOMMAND]

    TOOLSET_COMMAND [GLOBAL_OPTIONS] config [CONFIG_OPTIONS] [name [value]]

    TOOLSET_COMMAND [GLOBAL_OPTIONS] completion -- WORD
```

**TODO:**

* Subcommand `config` is not currently implemented.
* Subcommand `completion` is only partially implemented.


## External Subcommands

Following subcommands are included in Command Wrapper installation (this
package):

```
Like `cd` shell command, but allows the user to select a directory from a
pre-defined list.  Normally it spawns a new shell, or a Tmux window.

Usage:

  TOOLSET_COMMAND [GLOBAL_OPTIONS] cd [-s|--shell|-t|--tmux|-e|--terminal]

Available options:

  -h, --help                Show this help text
  -s, --shell               Execute a subshell even if in a Tmux session.
  -t, --tmux                Create a new Tmux window, or fail if not in Tmux
                            session.
  -e, --terminal            Open a new terminal emulator window.
```

```
Execute a command with predefined environment and command line options.

Usage:

  TOOLSET_COMMAND [GLOBAL_OPTIONS] exec COMMAND_NAME [--] [EXTRA_COMMAND_ARGUMENTS]

  TOOLSET_COMMAND [GLOBAL_OPTIONS] exec [-l|--ls]

Available options:
  -h, --help                Show this help text
  -l, --ls                  List available commands.
```

```
Generate subcommand skeleton for specific command-wrapper environment.

Usage:

  TOOLSET_COMMAND [GLOBAL_OPTIONS] skel SUBCOMMAND [-l LANGUAGE|--language=LANGUAGE]

Available options:
  -h, --help                         Show this help text

  -l LANGAUGE, --language=LANGUAGE   Choose programming language of subcommand
                                     skeleton

  SUBCOMMAND                         Name of the new subcommand
```


## Install

Install `command-wrapper`:

```Bash
git clone https://github.com/trskop/command-wrapper.git ~/.local/src/trskop/command-wrapper
mkdir -p ~/.local/lib/command-wrapper
stack --stack-yaml="${HOME}/.local/src/trskop/command-wrapper/stack.yaml" --local-bin-path="${HOME}/.local/lib/command-wrapper" install
mkdir ~/.config/command-wrapper
```

Define new toolset:

```Bash
toolset='INSERT_COMMAND_NAME_HERE'
ln -s ../.local/lib/command-wrapper/command-wrapper ~/bin/"${toolset}"
mkdir ~/.config/"${toolset}" ~/.local/lib/"${toolset}"
```


## Directory Layout

````
~/
├── .config/
│   ├── ...
│   ├── command-wrapper/
│   │   ├── default.dhall
│   │   ├── command-wrapper-${subcommand0}.dhall
│   │   ├── ...
│   │   └── command-wrapper-${subcommandN}.dhall
│   └── ${toolset}/
│       ├── default.dhall
│       ├── ${toolset}-${toolsetSubcommand0}.dhall
│       ├── ...
│       └── ${toolset}-${toolsetSubcommand0}.dhall
├── .local/
│   ├── ...
│   └── lib/
│       ├── ...
│       ├── command-wrapper/
│       │   ├── command-wrapper
│       │   ├── command-wrapper-${subcommand0}
│       │   ├── ...
│       │   └── command-wrapper-${subcommandN}
│       └── ${toolset}/
│           ├── ${toolset}-${toolsetSubcommand0}
│           ├── ...
│           └── ${toolset}-${toolsetSubcommandN}
└── bin/
    └── ${toolset} --> ../.local/lib/command-wrapper/command-wrapper
````



[Haskell.org]:
  http://www.haskell.org
  "The Haskell Programming Language"
[tl;dr Legal: BSD3]:
  https://tldrlegal.com/license/bsd-3-clause-license-%28revised%29
  "BSD 3-Clause License (Revised)"
