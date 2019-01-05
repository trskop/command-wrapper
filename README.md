# Command Wrapper

[![Haskell Programming Language](https://img.shields.io/badge/language-Haskell-blue.svg)][Haskell.org]
[![BSD3 License](http://img.shields.io/badge/license-BSD3-brightgreen.svg)][tl;dr Legal: BSD3]


## Description

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

In general such `TOOLSET_COMMAND` has syntax like this:

    TOOLSET_COMMAND [GLOBAL_OPTIONS] SUBCOMMAND [SUBCOMMAND_ARGUMENTS]

Multiple toolsets can easily coexist on the same machine.  It usually makes
sense to have one for personal tooling, and one for work tooling.

First subcommand that was introduced was `help`, obviously, but the one right
after that was `skel`.  Which allows you to create a new subcommand skeleton,
see `command-wrapper-skel(1)` manual page for more details.  Subcommand can be
written in any language user chooses.  It just needs to be an executable, and
follow Command Wrapper's [*SUBCOMMAND PROTOCOL*](#subcommand-protocol).


## Documentation

Documentation is in the form of manual pages written in Markdown and compiled
using `pandoc`.  See [`man/`](./man/) directory.

### Basic Usage

```
TOOLSET_COMMAND [GLOBAL_OPTIONS] SUBCOMMAND [SUBCOMMAND_ARGUMENTS]
TOOLSET_COMMAND [GLOBAL_OPTIONS] config [SUBCOMMAND]
TOOLSET_COMMAND [GLOBAL_OPTIONS] help [SUBCOMMAND]
TOOLSET_COMMAND [GLOBAL_OPTIONS] {-h|--help}
```

More can be found in [`command-wrapper(1)`](man/command-wrapper.1.md) manual
page, including list of external subcommands installed along with it.

### Subcommand Protocol

Is documented in [`command-wrapper-subcommand-protocol(7)`
](man/command-wrapper-subcommand-protocol.7.md) a separate manual page.


## Install

Install `command-wrapper`:

```Bash
git clone https://github.com/trskop/command-wrapper.git ~/.local/src/trskop/command-wrapper
~/.local/src/trskop/command-wrapper/install
mkdir ~/.config/command-wrapper
```

Define new toolset:

```Bash
toolset='INSERT_COMMAND_NAME_HERE'
ln -s ../.local/lib/command-wrapper/command-wrapper ~/bin/"${toolset}"
mkdir ~/.config/"${toolset}" ~/.local/lib/"${toolset}"
```


[Haskell.org]:
  http://www.haskell.org
  "The Haskell Programming Language"
[tl;dr Legal: BSD3]:
  https://tldrlegal.com/license/bsd-3-clause-license-%28revised%29
  "BSD 3-Clause License (Revised)"
