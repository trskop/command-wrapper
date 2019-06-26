# Command Wrapper

[![Haskell Programming Language](https://img.shields.io/badge/language-Haskell-blue.svg)][Haskell.org]
[![BSD3 License](http://img.shields.io/badge/license-BSD3-brightgreen.svg)][tl;dr Legal: BSD3]

Command Wrapper is a meta tool.  A tool to create tools with a nice and
consistent UI.  We call these tools **toolsets**, sets of commands with a common
theme, or a purpose.


## Table of Contents

*   [Description](#description)

*   [Some Interesting Features](#some-interesting-features)

*   [Documentation](#documentation)

    *   [What is Toolset](#what-is-toolset)
    *   [How To Choose a Toolset Name](#how-to-choose-a-toolset-name)
    *   [Available Subcommands](#available-subcommands)
    *   [Introducing New Subcommand](#introducing-new-subcommand)
    *   [Internal Subcommands](#internal-subcommands)
    *   [External Subcommands](#external-subcommands)

*   [Installation](#installation)


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
see [`command-wrapper-skel(1)`](man/command-wrapper-skel.1.md) manual page for
more details.  Subcommand can be written in any language user chooses.  It just
needs to be an executable, and follow Command Wrapper's Subcommand Protocol,
which is described in its own manual page
[`command-wrapper-subcommand-protocol(7)`
](man/command-wrapper-subcommand-protocol.7.md).


## Some Interesting Features

*   Command Wrapper makes heavy use of [Dhall](https://dhall-lang.org/)
    configuration language.  It goes as far as having Dhall interpreter
    integrated into its `config` (internal) subcommand.  It also provides very
    useful Dhall library [`dhall/CommandWrapper`](dhall/CommandWrapper/).

*   Command line completion for Bash, Fish, and Zsh.

*   Subcommands can be written in any language, they just need to respect
    Subcommand Protocol (described in [`command-wrapper-subcommand-protocol(7)`
    ](man/command-wrapper-subcommand-protocol.7.md) manual page).

*   Subcommands adhering to Subcommand Protocol get command line completion
    and help integration for free.

*   Subcommand skeletons for Bash and Haskell.  Subcommands in those languages
    can leverage libraries provided by Command Wrapper.

*   Subcommand aliases similar to Git aliases.  Command line completion
    understands aliases as well.

*   Very useful `exec` subcommand that provides shell aliases on steroids.  It
    can leverage Dhall to compose commands, and even safely share them via
    internet.  Its documentation is available in form of manual page
    [`command-wrapper-exec(1)`](man/command-wrapper-exec.1.md).

*   A lot more.


## Documentation

In this section we will focus only on basic and some interesting use cases.
Detailed documentation is in the form of manual pages written in Markdown and
compiled using `pandoc`.  See [`man/`](./man/) directory.

When installed these can be viewed using:

```
TOOLSET_COMMAND help --man [SUBCOMMAND|TOPIC]
```


### What is Toolset

Toolset is just a name-space for commands.  Easiest way to initialise a toolset
named `yx` is:

```
~/.local/lib/command-wrapper/command-wrapper config --init --toolset=yx
```

Which creates a bunch of configuration files in `~/.config/yx` and most
importantly a symbolic link pointing to `command-wrapper` executable.

Whenever something like `yx SUBCOMMAND` is executed, and `SUBCOMMAND` is not an
internal command (command implemented inside Command Wrapper), then Command
Wrapper will look for executable named `yx-${SUBCOMMAND}` to execute.

Command Wrapper looks for these commands in directories specified in its
configuration file, and if it fails, then it looks for them in directories
specified by `PATH` environment variable.


### How To Choose a Toolset Name

Good toolset name should be:

* Short
* Memorable (to you)
* Easy to type on your keyboard layout


### Available Subcommands

All available subcommands and aliases can be listed using:

```
TOOLSET_COMMAND completion --query --subcommands
```

To list only subcommands we can use following:

```
TOOLSET_COMMAND --no-aliases completion --query --subcommands
```

The list printed by the above command includes internal and external
subcommands.

To list only aliases we can use either:

```
TOOLSET_COMMAND completion --query --subcommand-aliases
```

Or we can use `help` to print not only their names, but also their short
descriptions:

```
TOOLSET_COMMAND help --aliases
```

Help for individual subcommands can be printed using

```
TOOLSET_COMMAND help SUBCOMMAND
```

If `SUBCOMMAND` has a manual page then following command will display it:

```
TOOLSET_COMMAND help --man SUBCOMMAND
```

Calling `TOOLSET_COMMAND help ALIAS` works as well, but it prints help for the
subcommand that will be invoked, not the alias itself.


### Introducing New Subcommand

Following command will create a skeleton of a new subcommand:

```
TOOLSET_COMMAND skel [--language={bash|haskell}] [--edit] SUBCOMMAND
```

If `--edit` is specified then it immediately opens the created file in an
editor.  Preferred editor and enabling `--edit` by default can be done in a
configuration file, see [`command-wrapper-skel(1)`
](man/command-wrapper-skel.1.md) manual page for more information.

By default subcommands written in Bash are stored under following name:

```
~/.local/lib/${toolset}/${toolset}-${subcommand}
```

In contrast Haskell subcommands are stored as:

```
~/.config/${toolset}/toolset/app-${toolset}-${subcommand}/Main.hs
```

These paths are configurable, see [`command-wrapper-skel(1)`
](man/command-wrapper-skel.1.md) manual page.  The reason for subcommands
written in Haskell to be named this way is that we expect
`~/.config/${toolset}/toolset/` to be a Haskell package so that code can be
shared among subcommands in the form of library.

Best approach is to create Bash script first, and later rewrite it using a
proper programming language.  The later step may never come, it depends on how
complex the functionality is.


### Internal Subcommands

Command Wrapper provides few internal commands that are intended to make user
experience nice, and to provide basic utilities to external subcommands.

```
TOOLSET_COMMAND [GLOBAL_OPTIONS] help [HELP_OPTIONS] [SUBCOMMAND]
TOOLSET_COMMAND [GLOBAL_OPTIONS] config [CONFIG_OPTIONS] [SUBCOMMAND]
TOOLSET_COMMAND [GLOBAL_OPTIONS] version [VERSION_OPTIONS]
TOOLSET_COMMAND [GLOBAL_OPTIONS] completion [COMPLETION_OPTIONS]
TOOLSET_COMMAND [GLOBAL_OPTIONS] {--version|-V}
TOOLSET_COMMAND [GLOBAL_OPTIONS] {--help|-h}
```

If `TOOLSET_COMMAND` is named `yx` then invoking help for it is:

```
yx help
```

More can be found in [`command-wrapper(1)`](man/command-wrapper.1.md) manual
page.  Individual internal subcommands are:

*   [`command-wrapper-help(1)`](man/command-wrapper-help.1.md) -- Display help
    message or manual page for Command Wrapper or one of its subcommands.

*   [`command-wrapper-config(1)`](man/command-wrapper-config.1.md) -- Command
    Wrapper's configuration swiss army knife.  It includes
    [Dhall](https://dhall-lang.org/) interpreter.

*   [`command-wrapper-version(1)`](man/command-wrapper-version.1.md) -- Print
    version information either in human readable format, or in a machine
    readable one.

*   [`command-wrapper-completion(1)`](man/command-wrapper-completion.1.md) --
    Command line completion toolbox.


### External Subcommands

Basic invocation:

```
TOOLSET_COMMAND [GLOBAL_OPTIONS] SUBCOMMAND [SUBCOMMAND_ARGUMENTS]
```

If our tooset is named `yx` and we are calling `cd` subcommand then this
becomes, for example:

```
yx cd
```

Command Wrapper installation comes with following commands that are implemented
as separate executables (external subcommands):

*   [`command-wrapper-cd(1)`](man/command-wrapper-cd.1.md)

*   [`command-wrapper-exec(1)`](man/command-wrapper-exec.1.md)

*   [`command-wrapper-skel(1)`](man/command-wrapper-skel.1.md)


## Installation

1.  Install `command-wrapper`:

    1.  Clone Git repository:

        ```Bash
        git clone https://github.com/trskop/command-wrapper.git ~/.local/src/github.com/trskop/command-wrapper
        ```

    2.  Build and install Command Wrapper binaries and documentation:

        ```Bash
        ~/.local/src/github.com/trskop/command-wrapper/install
        ```

    3.  Initialise Command Wrapper configuration:

        ```Bash
        ~/.local/lib/command-wrapper/command-wrapper config --init
        ```

2.  Define new toolset.

    In this example we'll name it `yx`, you can name it however you like:

    ```Bash
    ~/.local/lib/command-wrapper/command-wrapper config --init --toolset=yx
    ```

3.  Enable command line completion for new toolset.

    *   If you're using **Bash** then include following in your `~/.bashrc`:

        ```Bash
        source <(yx completion --script --shell=bash)
        ```

    *   If you're using **Fish** then include following in your
        `~/.config/fish/config.fish`, or add as
        `~/.config/fish/completions/yx.fish`:

        ```Bash
        yx completion --script --shell=fish | source
        ```

    *   If you're using **Zsh** then include following in your `~/.zshrc`, it has
        to be after `compinit` call:

        ```Bash
        source <(yx completion --script --shell=zsh)
        ```



[Haskell.org]:
  http://www.haskell.org
  "The Haskell Programming Language"
[tl;dr Legal: BSD3]:
  https://tldrlegal.com/license/bsd-3-clause-license-%28revised%29
  "BSD 3-Clause License (Revised)"
