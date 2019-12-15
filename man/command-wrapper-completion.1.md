% COMMAND-WRAPPER-COMPLETION(1) Command Wrapper 0.1.0 | Command Wrapper
% Peter Trsko
% 15th December 2019


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

TOOLSET\_COMMAND \[GLOBAL\_OPTIONS] completion \--library \[\--shell=*SHELL*]
\[\--import|\--content] \[\--output=*FILE*]

TOOLSET\_COMMAND \[GLOBAL\_OPTIONS] completion \--query \[\--output=*FILE*]
\[*QUERY\_OPTIONS*] \[\--algorithm=*ALGORITHM*] \[\--pattern=*PATTERN*]

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
:   Print a library to standard output that can be used by a subcommand.  In
    a subcommand implemented in Bash we can include a support library using:

    ```
    source <(COMMAND_WRAPPER_INVOKE_AS="${COMMAND_WRAPPER_NAME}" "${COMMAND_WRAPPER_EXE}" completion --library --shell=bash)
    ```

    Library itself is documented.  One can read through it by just printing it:

    ```
    TOOLSET completion --library [--shell=SHELL]
    ```

    Or piping it to pager:

    ```
    TOOLSET completion --library [--shell=SHELL] | less
    ```

    On Debian we can also use `sensible-pager` command instead of directly
    calling specific one.  Commands like `bat` or other `cat`-like tool with
    syntax highlighting support are also a great choice:

    ```
    TOOLSET completion --library --shell=bash | bat --language bash
    ```

    See also `command-wrapper-bash-library(7)` manual page.

\--shell=*SHELL*
:   Print library for *SHELL*.  Currently only supported value is *bash*.

\--import
:   Print code snipped for *SHELL* that imports Command Wrapper library for it.
    If neither `--content`, nor `--import` is specified then --content is
    assumed.

\--content
:   Print content of the Command Wrapper library for *SHELL*.  This is the
    default behavior if neither `--content`, nor `--import` is specified.


# QUERY OPTIONS

\--query
:   Query command line interface.  Useful for editor/IDE integration.

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

\--words \[\--] \[*WORD* \[...]]
:   Query matching words from *WORD* list.

\--algorithm=*ALGORITHM*
:   Specify which pattern matching *ALGORITHM* to use when `--pattern=`*PATTERN* is
    provided. Possible values are:

    *   *prefix* -- matches when *PATTERN* is a prefix of matched string.
    *   *fuzzy* -- matches when *PATTERN* fuzzily matches a string.
    *   *equality* -- matches when *PATTERN* exactly matches a string.

\--pattern=*PATTERN*
:   Print only values that are matching *PATTERN*.


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
