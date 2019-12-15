% COMMAND-WRAPPER-BASH-LIBRARY(7) Bash Library | v1.0.0
% Peter Trsko
% 15th December 2019


# NAME

Bash library for Command Wrapper subcommands written as Bash scripts.


# DESCRIPTION

Library that provides basic functions to build a Command Wrapper subcommand, in
Bash, that respects Command Wrapper Subcommand Protocol (see
`command-wrapper-subcommand-protocol(7)` manual page for details).


# LIBRARY DOCUMENTATION

Bash library is self-documented, the easiest way how to see the documentation
is to run (if you're using `less` as pager, if not read further):

```
TOOLSET_COMMAND completion --library --shell=bash [--content] | less
```

When we're not using `less` then changhe the last command in the pipeline to
be your prefered pager. If we're on Debian-like system then an also use
`sensible-pager` command instead of directly calling specific one.

If [`bat`](https://github.com/sharkdp/bat), or any other `cat`-like command
with syntax highlighting functionality is installed, then it is better to use:

```
TOOLSET_COMMAND completion --library --shell=bash [--content] | bat -l bash
```

Where `-l` is short for `--language` and allows `bat` to use syntax
highlighting for Bash.

For more information see `command-wrapper-completion(1)` manual page.


# IMPORTING THE LIBRARY

Command Wrapper provides snippet of code that describes the best way to import
its Bash library:

```
TOOLSET_COMMAND completion --library --shell=bash --import
```

For more information see `command-wrapper-completion(1)` manual page.


# SEE ALSO

command-wrapper(1), command-wrapper-completion(1),
command-wrapper-subcommand-protocol(7)

bat(1), less(1)

*   [`bat`](https://github.com/sharkdp/bat) is a variant of `cat` command with
    various improvements including `syntax-highlighting` and automatic pager
    support.


# BUGS

<https://github.com/trskop/command-wrapper/issues>
