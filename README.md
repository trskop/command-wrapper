# Command Wrapper

[![Haskell Programming Language](https://img.shields.io/badge/language-Haskell-blue.svg)][Haskell.org]
[![BSD3 License](http://img.shields.io/badge/license-BSD3-brightgreen.svg)][tl;dr Legal: BSD3]


## Description

Some command line applications with a lot of commands try to avoid poluting
`$PATH` with all of them. One of the approaches to this is to have one top
level command exposed and the rest is implemented as subcommands. Subcommands
are either internal functions or external commands (standalone executables).
Example of such application is Git which uses mix of internal subcommands and
external subcommand.

In general such toolset top level command has syntax like this:

    TOOLSET_COMMAND [GLOBAL_OPTIONS] SUBCOMMAND [SUBCOMMAND_OPTIONS]

This package provides universal top-level command, that can be named as
required, and API for subcommands. Subcommands may be written in any language,
they just need to be executable files that respect the subcommand API.


## Internal Subcommands


### help

```
Usage:

    TOOLSET_COMMAND [GLOBAL_OPTIONS] help [HELP_OPTIONS] [SUBCOMMAND]
```


### config

TODO: Not currently implemented.

```
Usage:

    TOOLSET_COMMAND [GLOBAL_OPTIONS] config [CONFIG_OPTIONS] [name [value]]
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
    └── ${toolset} --> ../.local/bin/command-wrapper
````



[Haskell.org]:
  http://www.haskell.org
  "The Haskell Programming Language"
[tl;dr Legal: BSD3]:
  https://tldrlegal.com/license/bsd-3-clause-license-%28revised%29
  "BSD 3-Clause License (Revised)"
