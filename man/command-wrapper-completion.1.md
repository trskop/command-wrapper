% COMMAND-WRAPPER-COMPLETION(1) Command Wrapper 0.1.0 | Command Wrapper
% Peter Trsko
% 4th January 2020


# NAME

`command-wrapper-completion` -- Command line completion, editor, and IDE
support.


# USAGE

TOOLSET\_COMMAND \[GLOBAL\_OPTIONS] completion \[\--index=*NUM*]
\[\--shell=*SHELL*] \[\--output=*FILE*] \[\--subcommand=*SUBCOMMAND*] \-- [*WORD* ...]

TOOLSET\_COMMAND \[GLOBAL\_OPTIONS] completion \--script \[\--shell=*SHELL*]
\[\--output=*FILE*] \[\--alias=*ALIAS* ...]

TOOLSET\_COMMAND \[GLOBAL\_OPTIONS] completion \--script \[\--shell=*SHELL*]
\[\--output=*FILE*] \--subcommand=*SUBCOMMAND* \--alias=*ALIAS* \[\--alias=*ALIAS* ...]

TOOLSET\_COMMAND \[GLOBAL\_OPTIONS] completion \--library
\[\--shell=*SHELL*|\--dhall=*LIBRARY*]
\[\--import|\--content] \[\--output=*FILE*]

TOOLSET\_COMMAND \[GLOBAL\_OPTIONS] completion \--query
{\--subcommands|\--subcommand-aliases|\--supported-shells|\--verbosity-values|\--colo\[u]r-values}
\[\--algorithm=*ALGORITHM*] \[\--pattern=*PATTERN*]
\[\--prefix=*STRING*] \[\--suffix=*STRING*]
\[\--output=*FILE*]

TOOLSET\_COMMAND \[GLOBAL\_OPTIONS] completion \--query \--file-system=*TYPE*
\[\--algorithm=*ALGORITHM*] \[\--pattern=*PATTERN*]
\[\--prefix=*STRING*] \[\--suffix=*STRING*]
\[\--output=*FILE*]

TOOLSET\_COMMAND \[GLOBAL\_OPTIONS] completion \--query \--words
\[\--algorithm=*ALGORITHM*] \[\--pattern=*PATTERN*]
\[\--prefix=*STRING*] \[\--suffix=*STRING*]
\[\--output=*FILE*]
\[\--] \[*WORD* ...]

TOOLSET\_COMMAND \[GLOBAL\_OPTIONS] completion \--wrapper
\--expression=*EXPRESSION* \--exec

TOOLSET\_COMMAND \[GLOBAL\_OPTIONS] completion {\--help|-h}

TOOLSET\_COMMAND \[GLOBAL\_OPTIONS] help completion


# DESCRIPTION

This subcommand provides support for command line completion (also known as TAB
completion).  At the moment only Bash is supported.

Another purpose of this command is editor and IDE support. It provides
interface for querying Command Wrapper's command line interface (CLI).


# COMMAND LINE COMPLETION OPTIONS

\--index=*NUM*
:   Position of a *WORD* for which we want completion.  In Bash this is the value
    of `COMP_CWORD` variable.

\--shell=*SHELL*
:   Provide completion or generate script for *SHELL*. Supported *SHELL* values
    are: *bash*, *fish*, and *zsh*.

\--subcommand=*SUBCOMMAND*
:   Do command line completion for a *SUBCOMMAND* instead.  This is useful for
    example when debugging command line completion of a *SUBCOMMAND*.

    ```
    $ TOOLSET_COMMAND completion --subcommand=version -- --he
    --help
    ```

    Value of *SUBCOMMAND* can be any internal subcommand, external subcommand,
    or an alias to one of those.

*WORD*
:   *WORD*s to complete. In Bash these are the elements of `COMP_WORDS` array.


# COMPLETION SCRIPT OPTIONS

\--script
:   Generate completion script suitable for sourcing in your shell's \*rc file.

\--shell=*SHELL*
:   Provide completion or generate script for *SHELL*.  Supported *SHELL* values
    are: *bash*, *fish*, and *zsh*.

\--subcommand=*SUBCOMMAND*
:   Generate completion script for a *SUBCOMMAND* instead of the whole toolset.
    At least one instance of `--alias=`*ALIAS* has to be specified:

    ```
    TOOLSET_COMMAND completion --script --subcommand=this --alias=this
    ```

    Completion will be generated for `this` command, therefore there should
    also be an alias for it defined in e.g. `.bashrc`:

    ```
    alias this='TOOLSET_COMMAND this'
    ```

\--alias=*ALIAS*
:   *ALIAS* under which Command Wrapper toolset is also known.  This is usually
    name of a shell alias, e.g. `alias ts=toolset` where ts is an *ALIAS* for
    which we want command line completion to work as well.


# SUBCOMMAND LIBRARY OPTIONS

\--library
:   Print a library to standard output that can be used by a subcommand, or a
    configuration file.  Option `--shell=`*SHELL* and `--dhall=`*LIBRARY*
    control which library is produced.  If neither `--shell=`*SHELL* nor
    `--dhall=`*LIBRARY* are specified then `--shell=bash` is assumed.

\--shell=*SHELL*
:   Print library for *SHELL*.  Currently only supported value is *bash*.

    In a subcommand implemented in Bash we can include a support library using
    a code snippet that is printed by following command:

    ```
    TOOLSET completion --library --shell=bash --import
    ```

    Library itself is documented.  One can read through it by just printing it:

    ```
    TOOLSET completion --library [--shell=SHELL]
    ```

    For more useful tips and additional documentation see also
    `command-wrapper-bash-library(7)` manual page.

\--dhall=*LIBRARY*
:   Print specified Dhall *LIBRARY*, or its import snippet when `--import` is
    specified.  Supported values of *LIBRARY* are:

    *   **prelude** -- Latest (known) version of Dhall prelude (v12.0.0).
    *   **prelude-v11.1.0**
    *   **prelude-v12.0.0**
    *   **command-wrapper** -- Latest (known) version of Command Wrapper's
        Dhall library.
    *   **exec** -- Latest (known) version of Command Wrapper's Exec library.

    Produced dhall libraries can be examined using following:

    ```
    TOOLSET completion --library --dhall=prelude | TOOLSET config --dhall-filter input.List.map
    ```

\--import
:   Print code snipped for importing *SHELL* or Dhall *LIBRARY*.

    If neither `--content`, nor `--import` is specified then --content is
    assumed.

\--content
:   Print content of the Command Wrapper library for *SHELL*.  This is the
    default behavior if neither `--content`, nor `--import` is specified.


# COMMON QUERY OPTIONS

\--query *QUERY_WHAT_OPTION*
:   Query command line interface.  Useful for editor/IDE integration.  To see
    what can be used for *QUERY_WHAT_OPTION* look at following sections:

    *   *QUERY COMMAND WRAPPER OPTIONS*
    *   *QUERY FILE SYSTEM OPTIONS*
    *   *QUERY WORDS OPTIONS*

\--algorithm=*ALGORITHM*
:   Specify which pattern matching *ALGORITHM* to use when `--pattern=`*PATTERN* is
    provided. Possible values are:

    *   *prefix* -- matches when *PATTERN* is a prefix of matched string.
    *   *fuzzy* -- matches when *PATTERN* fuzzily matches a string.
    *   *equality* -- matches when *PATTERN* exactly matches a string.

\--pattern=*PATTERN*
:   Print only values that are matching *PATTERN*.

\--prefix=*STRING*
:   Prepend *STRING* to every result entry.

\--suffix=*STRING*
:   Append *STRING* to every result entry.


# QUERY COMMAND WRAPPER OPTIONS

\--subcommands
:   Query all available subcommands.  This includes internal subcommands,
    external subcommands, and subcommand aliases.  See also global option
    `--no-aliases` in `command-wrapper(1)`.

\--subcommand-aliases
:   Query available subcommand aliases.  See also global option `--no-aliases`
    in `command-wrapper(1)`.

\--supported-shells
:   Query shells supported by command line completion.  Supported *SHELL* values
    at the moment are: *bash*, *fish*, and *zsh*.

\--verbosity-values
:   Query possible *VERBOSITY* values.  These can be set using global
    `--verbosity=VERBOSITY` option, or are passed down to subcommands via
    `COMMAND_WRAPPER_VERBOSITY` environment variable.

    See `TOOLSET_COMMAND help` or `command-wrapper(1)` for more information on
    `--verbosity=VERBOSITY` option, and `command-wrapper-subcommand-protocol(7)`
    regarding `COMMAND_WRAPPER_VERBOSITY` environment variable.

\--colo\[u]r-values
:   Query possible *WHEN* colour output values.  These can be set using global
    `--colo[u]r=WHEN` option, or are passed down to subcommands via
    `COMMAND_WRAPPER_COLOUR` environment variable.

    See `TOOLSET_COMMAND help` or `command-wrapper(1)` for more information on
    `--colo[u]r=WHEN` option, and `command-wrapper-subcommand-protocol(7)`
    regarding `COMMAND_WRAPPER_COLOUR` environment variable.


# QUERY FILE SYSTEM OPTIONS

\--file-system=*TYPE*
:   Query file system for entries of *TYPE*:

    *   *directory* -- List directories.
    *   *file* -- List entries that are not directories (includes normal files,
        and symbolic links).
    *   *executable* -- List executable files.
    *   *symlink* -- List symbolic links.

    In this mode `--algorithm=`*ALGORITHM* option is ignored, and it always
    behaves as if `--algorithm=prefix` was specified.  This may change in the
    future.


# QUERY WORDS OPTIONS

\--words
:   Query matching words from *WORD* list.

*WORD*
:   Potential completions that are matched against `--pattern=`*PATTERN*.


# COMPLETION WRAPPER OPTIONS

\--wrapper
:   Generate and execute a script for command line completion.  Useful when
    reusing existing completion scripts.

\--expression=*EXPRESSION*
:   Dhall *EXPRESSION* to be used to generate executable script.

\--exec
:   Execute generated script directly.


# COMMON OPTIONS

These options are shared by all the above usage modes.

\--output=*FILE*, -o *FILE*
:   Write output into *FILE* instead of standard output.


# OTHER OPTIONS

\--help, -h
:   Print this help and exit.  Same as `TOOLSET_COMMAND help completion`.


# EXIT STATUS

See `command-wrapper(1)` manual page section *EXIT STATUS*.


# FILES AND DIRECTORIES

See `command-wrapper(1)` manual page section *FILES AND DIRECTORIES*.


# ENVIRONMENT VARIABLES

See `command-wrapper(1)` manual page section *ENVIRONMENT VARIABLES*.


# BASH CONFIGURATION

This subcommand can generate Bash completion script, which can be sourced in
`~/.bashrc` file:

```
# shellcheck source=/dev/null
source <(toolset completion --script --shell=bash)
```

Where `toolset` is the name under which `command-wrapper` is used.  If any
aliases for `toolset` are used we can provide completion for them as well:

```
alias ts=toolset
# shellcheck source=/dev/null
source <(toolset completion --script --shell=bash --alias='ts')
```

It is also possible to get completion for an alias to a subcommand:

```
alias this='toolset this'
source <(toolset completion --script --subcommand=this --alias=this)
```

As an alternative to executing Command Wrapper to get completion script we can
use cached value instead:

```Bash
declare toolset='toolset'  # TODO: Set toolset name.
declare toolsetCacheDir="${XDG_CACHE_HOME:-${HOME}/.cache}/${toolset}"
declare toolsetCompletionFile="${toolsetCacheDir}/completion.bash"
if [[ ! -e "${toolsetCompletionFile}" ]]; then
    mkdir -p "${toolsetCacheDir}"

    # Using temporary file prevents us from having a conflict when two shells
    # are started and there is no completion file.  We are relying on the fact
    # that 'mv' operation is atomic, therefore, all shells will see consistent
    # version of completion file.
    #
    # Since this is `~/.bashrc` we cannot rely on `set -e` to handle errors for
    # us, hence the `&&` chain.

    declare toolsetCompletionTempFile
    toolsetCompletionTempFile="$(
        mktemp --tmpdir="${toolsetCacheDir}" --suffix=.bash completion.XXXXXXXXXX
    )" \
        && "${toolset}" completion --script --shell=bash --output="${toolsetCompletionTempFile}" \
        && mv --force "${toolsetCompletionTempFile}" "${toolsetCompletionFile}"
fi

source "${toolsetCompletionFile}"
unset -v toolset toolsetCacheDir toolsetCompletionFile toolsetCompletionTempFile
```

In the above example we used `toolset` as an variable so that it can be more
easily copy-pasted.


# FISH CONFIGURATION

Include following in your `~/.config/fish/config.fish`, or add as
`~/.config/fish/completions/yx.fish`:

```
toolset completion --script --shell=fish | source
```

Where `toolset` is the name under which `command-wrapper` is used.  If any
aliases for `toolset` are used we can provide completion for them as well:

```
alias ts toolset
toolset completion --script --shell=fish --alias='ts' | source
```

It is also possible to get completion for an alias to a subcommand:

```
alias this 'toolset this'
toolset completion --script --shell=fish --subcommand=this --alias=this | source
```


# ZSH CONFIGURATION

Include following in your `~/.zshrc`, it has to be after `compinit` call:

```Bash
source <(toolset completion --script --shell=zsh)
```

Where `toolset` is the name under which `command-wrapper` is used.  If any
aliases for `toolset` are used we can provide completion for them as well:

```
alias ts=toolset
source <(toolset completion --script --shell=bash --alias='ts')
```

It is also possible to get completion for an alias to a subcommand:

```
alias this='toolset this'
source <(toolset completion --script --subcommand=this --alias=this)
```


# SEE ALSO

command-wrapper(1), command-wrapper-bash-library(7),
command-wrapper-subcommand-protocol(7)


# BUGS

<https://github.com/trskop/command-wrapper/issues>
