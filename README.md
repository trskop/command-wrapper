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

    -   [What is a Toolset](#what-is-a-toolset)
    -   [How To Choose a Toolset Name](#how-to-choose-a-toolset-name)
    -   [List Available Subcommands](#list-available-subcommands)
    -   [Introducing New Subcommand](#introducing-new-subcommand)
    -   [Internal Subcommands](#internal-subcommands)
    -   [External Subcommands](#external-subcommands)
    -   [Subcommand Aliases](#subcommand-aliases)
    -   [Command Line Completion](#command-line-completion)
        *   [Bash Command Line Completion](#bash-command-line-completion)
        *   [Command Line Completion For Aliases](#command-line-completion-for-aliases)

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

*   Command Wrapper makes heavy use of [Dhall][Dhall homepage]
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

In this section we will focus only on basics, and some interesting use cases.
Detailed documentation is in the form of manual pages written in Markdown and
compiled using `pandoc`.  See [`man/`](./man/) directory.

When installed these can be viewed using:

```
TOOLSET_COMMAND help --man [SUBCOMMAND|TOPIC]
```

While not necessary, it is highly encouraged to read about [Dhall
][Dhall homepage] configuration language too.  Knowledge of its syntax will
help with understanding some of the examples that are provided in this section.


### What is a Toolset

Toolset is just a name-space for commands.  Easiest way to initialise a toolset
named `yx` (arbitrarily choosen name) is:

```Bash
~/.local/lib/command-wrapper/command-wrapper config --init --toolset=yx
```

Which creates a bunch of configuration files in `~/.config/yx`, and most
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


### List Available Subcommands

All available subcommands and aliases can be listed using:

```
TOOLSET_COMMAND completion --query --subcommands
```

To list only subcommands, omitting aliases (see [Subcommand Aliases
](#subcommand-aliases) section), we can use following:

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

```Bash
~/.local/lib/${toolset}/${toolset}-${subcommand}
```

In contrast Haskell subcommands are stored as:

```Bash
~/.config/${toolset}/toolset/app-${toolset}-${subcommand}/Main.hs
```

These paths are configurable, see [`command-wrapper-skel(1)`
](man/command-wrapper-skel.1.md) manual page.  The reason for subcommands
written in Haskell to be named this way is that we expect
`~/.config/${toolset}/toolset/` to be a Haskell package so that code can be
shared among subcommands in the form of a library.

Best approach is to create Bash script first, and later rewrite it using a
proper programming language.  The later step may never come, it depends on how
complex the functionality is.

Subcommands written in Bash can use provided `bash/lib.bash` library.  Code
snippet for importing it can be obtained by:

```
TOOLSET_COMMAND completion --library --shell=bash --import
```

Bash library is self documented, to view its content we can use following
command:

```
TOOLSET_COMMAND completion --library --shell=bash --content
```

If e.g. [`bat`][bat repo] (a `cat` clone with syntax highlighting and pager
support) we can browse the library quite comfortably with:

```
TOOLSET_COMMAND completion --library --shell=bash --content | bat -l bash
```

More about Bash library can be found in a dedicated manual page
[`command-wrapper-bash-library(7)`](man/command-wrapper-bash-library.7.md).


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
    [Dhall][Dhall homepage] interpreter.

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
becomes:

```
yx cd [CD_ARGUMENTS]
```

When Command Wrapper is looking for external commands it searches through these
directories in specified order:

1.  Directories that are specified in its global configuration file
    `${XDG_CONFIG_HOME:-${HOME}/.config}/command-wrapper/default.dhall`.  By
    default this contains only one directory, which is
    `${HOME}/.local/lib/command-wrapper/`.
2.  Directories specified in toolset specific configuration file
    `${XDG_CONFIG_HOME:-${HOME}/.config}/${toolset}/default.dhall`.  By default
    only `${HOME}/.local/lib/${toolset}` directory is configured here.
3.  Directories specified in `COMMAND_WRAPPER_PATH` environment variable, which
    contains list of directories separated by `:` character.  This environment
    variable is here to allow experimentation.
4.  Directories specified in `PATH` environment variable.

Command Wrapper installation comes with following commands that are implemented
as separate executables (external subcommands):

*   [`command-wrapper-cd(1)`](man/command-wrapper-cd.1.md)

*   [`command-wrapper-exec(1)`](man/command-wrapper-exec.1.md)

*   [`command-wrapper-skel(1)`](man/command-wrapper-skel.1.md)


### Subcommand Aliases

Command Wrapper allows us to define aliases in a similar way as Git.  These are
defined either in `~/config/${toolset}/default.dhall` (global aliases, shared
by all toolsets) or in `~/config/${toolset}/default.dhall` (toolset specific
aliases).  An alias looks like this:

```Dhall
let CommandWrapper =
      https://raw.githubusercontent.com/trskop/command-wrapper/master/dhall/CommandWrapper/package.dhall

in  CommandWrapper.SubcommandAlias::{
    , alias = "man"
    , description = Some "Shorthand for \"help --man\"."
    , command = "help"
    , arguments = [ "--man" ]
    }
```

To see definition of `SubcommandAlias` type and default values see
[`dhall/CommandWrapper/SubcommandAlias`
](./dhall/CommandWrapper/SubcommandAlias).

Invoking such alias looks like any other subcommand invocation:

```
TOOLSET_COMMAND man [SUBCOMMAND|TOPIC]
```

And it is the same as calling:

```
TOOLSET_COMMAND help --man [SUBCOMMAND|TOPIC]
```

The `description` field is printed when listing aliases using:

```
TOOLSET_COMMAND help --aliases
```

Command line completion, and other standard subcommand goodies, work for
aliases as well, however, one must be aware that calling `help` on an alias
gives help for the underlying command.  In case of our `man` alias it would be
the same as if we called:

```
TOOLSET_COMMAND help help
```

For more information see [`command-wrapper(1)`](man/command-wrapper.1.md) which
provides more information on how aliases are defined.  To see what aliases are
defined by default when
`~/.local/lib/command-wrapper/command-wrapper config --init` is invoked
(described more in [Installation](#installation) section) see
[`dhall/init/command-wrapper/default/aliases-common.dhall`
](./dhall/init/command-wrapper/default/aliases-common.dhall).


### Command Line Completion

All subcommands have to provide command line completion.  This is a requirement
specified in [`command-wrapper-subcommand-protocol(7)`
](man/command-wrapper-subcommand-protocol.7.md).  The benefit of the approach
that is documented in mentioned Subcommand Protocol is that there is no need
to hook it up into our shell if our toolset is already.

Command Wrapper provides its own script for enabling command line completion
in Bash, Fish or Zsh:

```
TOOLSET_COMMAND completion --script [--shell=SHELL] [--output=FILE] [--alias=ALIAS ...]
```

There are multiple ways of hooking toolset command line completion into your
shell.  Here we are listing few of them, please, go read them all and choose
the one that fits your use case the best.


#### Bash Command Line Completion

*   **Sourcing generated script every time.**  For example, enabling it for Bash means adding
    following line into `~/.bashrc`:

    ```Bash
    source <("${toolset}" completion --script --shell=bash)
    ```

*   **Sourcing cached script.**  The above approach has the
    disadvantage that every time we start a shell we need to run Command
    Wrapper to get the completion script.  This can be avoided by writting it
    somewhere.  For example we can do following in `~/.bashrc`:

    ```Bash
    declare toolset='toolset'  # TODO: Set toolset name.
    declare toolsetCacheDir="${XDG_CACHE_HOME:-${HOME}/.cache}/${toolset}"
    declare toolsetCompletionFile="${toolsetCacheDir}/completion.bash"
    if [[ ! -e "${toolsetCompletionFile}" ]]; then
        mkdir -p "${toolsetCacheDir}"

        # This relies on the fact that Command Wrapper uses atomic write
        # operation to create the output file.
        "${toolset}" completion --script --shell=bash \
            --output="${toolsetCompletionFile}"
    fi

    source "${toolsetCompletionFile}"
    unset -v toolset toolsetCacheDir toolsetCompletionFile
    ```

    If there is ever a change in how the generated script looks like you'll
    need to remove following file manually.  Following is an example of Bash
    session where `user@machine:~$` indicates command line prompt (**Don't
    forget to use your toolset name**):

    ```Bash
    user@machine:~$ toolset='toolset'  # TODO: Set toolset name.
    user@machine:~$ rm "${XDG_CACHE_HOME:-${HOME}/.cache}/${toolset}/completion.bash"
    ```

*   **Storing generated script.**  We can store the generated script ourselves and
    source it in `~/.bashrc`, this way our Bash rc-file stays simple.
    Following is an example of Bash session where `user@machine:~$` indicates
    command line prompt (**Don't forget to use your toolset name**):

    ```Bash
    user@machine:~$ toolset='toolset'  # TODO: Set toolset name.
    user@machine:~$ mkdir -p "${XDG_CACHE_HOME:-${HOME}/.cache}/${toolset}/"
    user@machine:~$ "${toolset}" completion --script --shell=bash --output="${XDG_CACHE_HOME:-${HOME}/.cache}/${toolset}/completion.bash"
    ```

    After that we can just source it in our `~/.bashrc`:

    ```
    # shellcheck source=/dev/null
    source "${XDG_CACHE_HOME:-${HOME}/.cache}/${toolset}/completion.bash"
    ```

    If there is ever a change in how the generated script looks like you'll
    need to manually recreate completion script
    (`${XDG_CACHE_HOME:-${HOME}/.cache}/${toolset}/completion.bash`).

*   **Inlining generated script.**  This can be doen by writting output of
    `"${toolset}" completion --script --shell=bash` your `~/.bashrc`.
    Following is an example of Bash session where `user@machine:~$` indicates
    command line prompt (**Don't forget to use your toolset name, and use `>>`
    operator to append, not overwrite!**):

    ```Bash
    user@machine:~$ "${toolset}" completion --script --shell=bash >> ~/.bashrc
    ```

    If there is ever a change in how the generated script looks like you'll
    need to remove that code from your `~/.bashrc` and reinsert it.


#### Command Line Completion For Aliases

If we are invoking our toolset under multiple names (only one name can be
tooset name, rest are shell aliases) then we can generate completion for all of
them.  Examples in this section are for Bash, it should be easy to adapt them
to other shells.

For example if our toolset is called `habit` and we use `hb` as an alias we can
enable completion for both:

```Bash
alias hb=habit
source <(habit completion --script --shell=bash --alias=hb)
```

If we want to define shell alias for a subcommand we will need to enable
command line completion in our shell.  For example if our toolset is named
`habit`, and we want to use its Dhall interpreter under the name `dhall`.
Doing this consists of two steps:

1.  Define an alias for `config --dhall`:

    ```Dhall
    let CommandWrapper =
          https://raw.githubusercontent.com/trskop/command-wrapper/master/dhall/CommandWrapper/package.dhall

    in  CommandWrapper.SubcommandAlias::{
        , alias = "dhall"
        , description = Some "Shorthand for \"config --dhall\"."
        , command = "config"
        , arguments = [ "--dhall" ]
        }
    ```

    See also [Subcommand Aliases](#subcommand-aliases) section.

2.  Add following into `~/.bashrc`:

    ```Bash
    alias dhall='habit dhall'
    source <(habit completion --script --shell=bash --subcommand=dhall --alias=dhall)
    ```

The reason why we can't use `alias dhall='habit config --dhall'` is that, at
the moment, Command Wrapper has no way of knowing that we are passing `--dhall`
option to `config` command.  Therefore, command line completion would be for
`config` subcommand, and not for `config --dhall`.


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

    *   If you're using **Bash** then fastest way is to include following in your
        `~/.bashrc`:

        ```Bash
        source <(yx completion --script --shell=bash)
        ```

        There are other methods to consider, please read
        [Bash Command Line Completion](#bash-command-line-completion) section
        for more information.

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


[Dhall homepage]:
  https://dhall-lang.org/
  "The Dhall configuration language homepage"
[bat repo]:
  https://github.com/sharkdp/bat
  "bat GitHub repository"
[Haskell.org]:
  http://www.haskell.org
  "The Haskell Programming Language"
[tl;dr Legal: BSD3]:
  https://tldrlegal.com/license/bsd-3-clause-license-%28revised%29
  "BSD 3-Clause License (Revised)"
