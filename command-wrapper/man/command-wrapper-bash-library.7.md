% COMMAND-WRAPPER-BASH-LIBRARY(7) Bash Library | v1.0.0
% Peter Trsko
% 19th March 2020


# NAME

Bash library for Command Wrapper subcommands written as Bash scripts.


# DESCRIPTION

Library that provides basic functions to build a Command Wrapper subcommand, in
Bash, that respects Command Wrapper Subcommand Protocol (see
`command-wrapper-subcommand-protocol(7)` manual page for details).

Some of the functions provided by the library are documented in this manual
page, however, it is not in any way comprehensive documentation.   See
*LIBRARY DOCUMENTATION* section on how to access documentation embedded in the
library.


# IMPORTING THE LIBRARY

Command Wrapper provides snippet of code that describes the best way to import
its Bash library.  Paste output of the following command into your script:

```
TOOLSET_COMMAND completion --library --shell=bash --import
```

For more information see `command-wrapper-completion(1)` manual page.

In Vim/Neovim you can use following ex command to insert the code snippet
(don't forget to substitute `TOOLSET_COMMAND` with what you're using):

```
:r!TOOLSET_COMMAND completion --library --shell=bash --import
```

When subcommand is created via standard `skel` template, like:

```
TOOLSET_COMMAND skel --language=bash SUBCOMMAND
```

Then the created Bash script should already handle the library import
correctly.  For more information see `command-wrapper-skel(1)` manual page.


# LIBRARY DOCUMENTATION

Bash library is self-documented, the easiest way how to see the documentation
is to run (if you're using `less` as pager, if not read further):

```
TOOLSET_COMMAND completion --library --shell=bash [--content] | less
```

When we're not using `less` then change the last command in the pipeline to be
your preferred pager. If we're on Debian-like system then you can also use
`sensible-pager` command instead of directly calling specific one.

If [`bat`](https://github.com/sharkdp/bat), or any other `cat`-like command
with syntax highlighting functionality is installed, then it is better to use:

```
TOOLSET_COMMAND completion --library --shell=bash [--content] | bat -l bash
```

Where `-l` is short for `--language` and allows `bat` to use syntax
highlighting for Bash.

For more information see `command-wrapper-completion(1)` manual page.


# INFO, NOTICE, WARN, and ERROR MESSAGES

Print info/notice/warn/error messages to `stderr`.  Verbosity level (see
`COMMAND_WRAPPER_VERBOSITY` in `command-wrapper-subcommand-protocol(7)` for
details) affects if the message will actually be printed or not.

info *FORMAT* \[*ARGUMENTS*]

notice *FORMAT* \[*ARGUMENTS*]

warn *FORMAT* \[*ARGUMENTS*]

error *FORMAT* \[*ARGUMENTS*]


# OUTPUT MESSAGES

Output messages are slightly different from the info/notice/warn/error
messages. They are printed to `stdout`, without any extra formatting or
colours, but they are suppressed if verbosity is set to *silent*.

out *FORMAT* \[*ARGUMENTS*]


# ERROR HANDLING

Common thing is to print an error and exit wit a specific exit status.  For
this purpose we provide `die` function:

die *EXIT_CODE* *FORMAT* \[*ARGUMENTS*]


# CALL CURRENT COMMAND WRAPPER TOOLSET

In here `toolset` is literally name of the function that is provided by the
Bash library.  This way subcommand does not need to know what toolset it
belongs to.

toolset \[*GLOBAL_OPTIONS*] *SUBCOMMAND* \[\--] \[*SUBCOMMAND_ARGUMENTS*]

toolset {\--help|-h}

toolset {\--version|-V}


# DHALL

Command Wrapper's embedded Dhall functionality is also exposed via functions.
For more information see `command-wrapper-config(1)`.  Provided Dhall functions
are:

dhall
  \[\--\[no-]allow-imports] \[\--\[no-]cache]
  \[\--\[no-]annotate] \[\--\[no-]alpha] \[\--\[no-]type]
  \[\--let=NAME=*EXPRESSION*]
  \[\--expression=*EXPRESSION*|\--expression *EXPRESSION*|\--input=*FILE*|\--input *FILE*|-i *FILE*]
  \[\--output=*FILE*|\--output *FILE*|-o *FILE*]

dhall-filter
  \[\--\[no-]allow-imports] \[\--\[no-]cache]
  \[\--let=NAME=*EXPRESSION*]
  \[\--expression=*EXPRESSION*|\--expression *EXPRESSION*|\--input=*FILE*|\--input *FILE*|-i *FILE*]
  \[\--output=*FILE*|\--output *FILE*|-o *FILE*]
  *EXPRESSION*

dhall-format
  \[\--expression=*EXPRESSION*|\--expression *EXPRESSION*|\--input=*FILE*|\--input *FILE*|-i *FILE*]
  \[\--output=*FILE*|\--output *FILE*|-o *FILE*]

dhall-freeze
  \[\--\[no-]remote-only]
  \[\--for-security|\--for-caching]
  \[\--expression=*EXPRESSION*|\--expression *EXPRESSION*|\--input=*FILE*|\--input *FILE*|-i *FILE*]
  \[\--output=*FILE*|\--output *FILE*|-o *FILE*]

dhall-hash
  \[\--\[no-]cache]
  \[\--expression=*EXPRESSION*|\--expression *EXPRESSION*|\--input=*FILE*|\--input *FILE*|-i *FILE*]
  \[\--output=*FILE*|\--output *FILE*|-o *FILE*]

dhall-to-bash
  \[\--\[no-]allow-imports] \[\--\[no-]cache]
  \[\--declare=NAME]
  \[\--expression=*EXPRESSION*|\--expression *EXPRESSION*|\--input=*FILE*|\--input *FILE*|-i *FILE*]
  \[\--output=*FILE*|\--output *FILE*|-o *FILE*]

dhall-to-text
  \[\--\[no-]allow-imports] \[\--\[no-]cache]
  \[\--list]
  \[\--expression=*EXPRESSION*|\--expression *EXPRESSION*|\--input=*FILE*|\--input *FILE*|-i *FILE*]
  \[\--output=*FILE*|\--output *FILE*|-o *FILE*]

dhall-exec
  \[\--expression=*EXPRESSION*|\--expression *EXPRESSION*|\--input=*FILE*|\--input *FILE*|-i *FILE*]
  \[\--interpreter=*COMMAND* \[\--interpreter-argument=*ARGUMENT* ...]]
  \[*ARGUMENT* ...]


# EDIT FILE IN EDITOR

Calling user-preferred editor is something that Command Wrapper provides to
subcommands.  This functionality is documented in `command-wrapper-config(1)`
and is exposed as:

edit-file \[*FILE*|\--subcommand-config *SUBCOMMAND*]


# SELECTION MENU

When we need user to select a value using some kind of menu then we can once
again use functionality of Command Wrapper that is documented in
`command-wrapper-config(1)`.  To subcommand it is made available as:

select-menu \[\--input=*FILE*|\--arguments \[*STRING* ...]]


# COMPLETION QUERIES

Huge part of Command Wrapper functionality is command line completion.  It is
documented in its own `command-wrapper-completion(1)` manual page.
Most important functionality for subcommands is exposed as:

completion-query {\--subcommands|\--subcommand-aliases}
  \[\--algorithm=*ALGORITHM*] \[\--pattern=*PATTERN*] \[\--prefix=*STRING*]
  \[\--suffix=*STRING*] \[\--output=*FILE*]

completion-query {\--supported-shells|\--verbosity-values|\--colo\[u]r-values}
  \[\--algorithm=*ALGORITHM*] \[\--pattern=*PATTERN*] \[\--prefix=*STRING*]
  \[\--suffix=*STRING*] \[\--output=*FILE*]

completion-query \--file-system=TYPE
  \[\--\[no-]tilde-expansion] \[\--\[no-]substitute-tilde]
  \[\--algorithm=*ALGORITHM*] \[\--pattern=*PATTERN*] \[\--prefix=*STRING*]
  \[\--suffix=*STRING*] \[\--output=*FILE*]

completion-query \--words
  \[\--algorithm=*ALGORITHM*] \[\--pattern=*PATTERN*] \[\--prefix=*STRING*]
  \[\--suffix=*STRING*] \[\--output=*FILE*]
  \[\--] \[*WORD* ...]

If speed of completion is of an essence then Bash's `comgpgen`/`complete`
builtins should be preferred, however, some of the above cannot be reliably
emulated using Bash builtins.


# SUBCOMMAND PROTOCOL HELPERS

Check that the subcommand was called via Commnad Wrapper Subcommand Protocol
(documented in `command-wrapper-subcommand-protocol(7)`) or die with
appropriate error message and exit status:

dieIfExecutedOutsideOfCommandWrapperEnvironment \[*MIN_VERSION*]

Subcommands are required to implement `--completion-info` option which prints
Dhall expression that describes how subcommand should be called to do
completion.  If subcommand uses standard Commnad Wrapper-style calling
convention then following helper can be used to print respective Dhall
expression:

stdCompletionInfo \[\--hash]

Subcommand Protocol uses environment variables to pass information to a
subcommand.  If subcommand executes another command without modifying
environment then these are passed to that command as well.  Following helper
removes Subcommand Protocol environment variables before executing a command:

exec\_ \[-cl] \[-a *NAME*] \[*COMMAND* \[*ARGUMENTS* ...]] \[*REDIRECTION* ...]

Helper function that tests if configuration is available or not:

haveConfiguration \[\--or-die \[*FORMAT* \[*ARGUMENTS*]]]


# SEE ALSO

command-wrapper(1), command-wrapper-completion(1), command-wrapper-config(1),
command-wrapper-skel(1), command-wrapper-subcommand-protocol(7)

bat(1), less(1)

*   [`bat`](https://github.com/sharkdp/bat) is a variant of `cat` command with
    various improvements including `syntax-highlighting` and automatic pager
    support.


# BUGS

<https://github.com/trskop/command-wrapper/issues>
