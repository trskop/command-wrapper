% COMMAND-WRAPPER-CONFIG(1) Command Wrapper 0.1.0 | Command Wrapper
% Peter Trsko
% 15th December 2019


# NAME

`command-wrapper-config` -- Initialise, query, and update Command Wrapper
toolset configuration.


# USAGE

TOOLSET\_COMMAND \[GLOBAL\_OPTIONS] config \--dhall
\[\--\[no-]allow-imports|\--\[no-]alpha|\--\[no-]annotate|\--\[no-]type|\--\[no-]cache]
\[\--let=*NAME*=*EXPRESSION* ...]
\[\--expression=*EXPRESSION*|\--expression *EXPRESSION*|\--input=*FILE*|\--input *FILE*|-i *FILE*]
\[\--output=*FILE*|\--output *FILE*|-o *FILE*]

TOOLSET\_COMMAND \[GLOBAL\_OPTIONS] config \--dhall-filter
\[\--\[no-]cache]
\[\--\[no-]allow-imports]
\[\--let=*NAME*=*EXPRESSION* ...]
\[\--expression=*EXPRESSION*|\--expression *EXPRESSION*|\--input=*FILE*|\--input *FILE*|-i *FILE*]
\[\--output=*FILE*|\--output *FILE*|-o *FILE*]
*EXPRESSION*

TOOLSET\_COMMAND \[GLOBAL\_OPTIONS] config \--dhall-format
\[\--expression=*EXPRESSION*|\--expression *EXPRESSION*|\--input=*FILE*|\--input *FILE*|-i *FILE*]
\[\--output=*FILE*|\--output *FILE*|-o *FILE*]

TOOLSET\_COMMAND \[GLOBAL\_OPTIONS] config \--dhall-lint
\[\--expression=*EXPRESSION*|\--expression *EXPRESSION*|\--input=*FILE*|\--input *FILE*|-i *FILE*]
\[\--output=*FILE*|\--output *FILE*|-o *FILE*]

TOOLSET\_COMMAND \[GLOBAL\_OPTIONS] config \--dhall-resolve
\[\--\[no-]cache]
\[\--list-imports=*KIND*]
\[\--expression=*EXPRESSION*|\--expression *EXPRESSION*|\--input=*FILE*|\--input *FILE*|-i *FILE*]
\[\--output=*FILE*|\--output *FILE*|-o *FILE*]

TOOLSET\_COMMAND \[GLOBAL\_OPTIONS] config \--dhall-freeze
\[\--\[no-]remote-only]
\[\--for-security|\--for-caching]
\[\--expression=*EXPRESSION*|\--expression *EXPRESSION*|\--input=*FILE*|\--input *FILE*|-i *FILE*]
\[\--output=*FILE*|\--output *FILE*|-o *FILE*]

TOOLSET\_COMMAND \[GLOBAL\_OPTIONS] config \--dhall-diff *EXPRESSION* *EXPRESSION*
\[\--output=*FILE*|\--output *FILE*|-o *FILE*]

TOOLSET\_COMMAND \[GLOBAL\_OPTIONS] config \--dhall-repl

TOOLSET\_COMMAND \[GLOBAL\_OPTIONS] config \--dhall-hash
\[\--\[no-]cache]
\[\--expression=*EXPRESSION*|\--expression *EXPRESSION*|\--input=*FILE*|\--input *FILE*|-i *FILE*]
\[\--output=*FILE*|\--output *FILE*|-o *FILE*]

TOOLSET\_COMMAND \[GLOBAL\_OPTIONS] config \--dhall-bash
\[\--\[no-]allow-imports|\--\[no-]cache]
\[\--declare=*NAME*]
\[\--expression=*EXPRESSION*|\--input=*FILE*|\--input *FILE*|-i *FILE*]
\[\--output=*FILE*|\--output *FILE*|-o *FILE*]

TOOLSET\_COMMAND \[GLOBAL\_OPTIONS] config \--dhall-text
\[\--\[no-]allow-imports|\--\[no-]cache]
\[\--list]
\[\--expression=*EXPRESSION*|\--input=*FILE*|\--input *FILE*|-i *FILE*]
\[\--output=*FILE*|\--output *FILE*|-o *FILE*]

TOOLSET\_COMMAND \[GLOBAL\_OPTIONS] config \--dhall-exec
{\--expression=*EXPRESSION*|\--input=*FILE*|\--input *FILE*|-i *FILE*}
\[{\--interpreter=*COMMAND*|\--interpreter *COMMAND*} \[{\--interpreter-argument=*ARGUMENT*|\--interpreter-argument *ARGUMENT*} ...]]
[*ARGUMENT* ...]

TOOLSET\_COMMAND \[GLOBAL\_OPTIONS] config \--init
\[\--toolset=*NAME*|\--toolset *NAME*]

TOOLSET\_COMMAND \[GLOBAL\_OPTIONS] config {\--edit|-e}
\[*FILE*|\--subcommand-config *SUBCOMMAND*]

TOOLSET\_COMMAND \[GLOBAL\_OPTIONS] config \--manu
{\--input=*FILE*|\--input *FILE*|-i *FILE*|\--arguments \[*STRING* \[...]]}

TOOLSET\_COMMAND \[GLOBAL\_OPTIONS] config {\--help|-h}

TOOLSET\_COMMAND \[GLOBAL\_OPTIONS] help config

**TODO: Following usages aren't currently implemented!**

TOOLSET\_COMMAND \[GLOBAL\_OPTIONS] config \[OPTIONS] \[\--] \[*EXPRESSION*]


# DESCRIPTION

Command Wrapper's configuration swiss army knife.  On of its many purposes is
to provide Dhall functionality without the need to install external tools.

We can organise `config` subcommand abilities into following categories:

**Dhall** (`--dhall*`)
:   All the functionality of:

    *   [dhall](http://hackage.haskell.org/package/dhall) (**close to full
        support**)
    *   [dhall-bash](http://hackage.haskell.org/package/dhall-bash)
    *   [dhall-json](http://hackage.haskell.org/package/dhall-json) (**TODO**)
    *   [dhall-text](http://hackage.haskell.org/package/dhall-text)

    Which is integrated with Command Wrapper and with a nicer command line UX.

    Extra Dhall functionality:

    \--dhall-exec
    :   Execute rendered Dhall expression as a script.  This is done in following steps:

        1.  Dhall expression of type Text is parsed and rendered into text.

        2.  Text is written into a file
            `${XDG_CACHE_HOME:-${HOME}/.cache}/${toolset}-dhall-exec/${hash}`,
            where `${hash}` is a SHA256 of its content.

        3.  Script is executed:

            1.  If `--interpreter=`*COMMAND* is passed then it is executed as:

                ```
                INTERPRETER_COMMAND [INTERPRETER_ARGUMENTS] SCRIPT_FILE [SCRIPT_ARGUMENTS]
                ```

            2.  If no `--interpreter=`*COMMAND* is specified then executable
                bit is set on it and script is executed as:

                ```
                SCRIPT_FILE [SCRIPT_ARGUMENTS]
                ```

        Some examples:

        ```
        $ TOOLSET_COMMAND config --dhall-exec --expression='"#!/bin/bash\necho Hello World"'
        Hello World
        $ TOOLSET_COMMAND config --dhall-exec --expression='"echo Hello World"' --interpreter=bash
        Hello World
        ```

**Selection Menu** (`--menu`)
:   Display input in a menu to select an item.  Usage examples:

    ```Bash
    fd --type file | TOOLSET_COMMAND config --menu
    ```

    ```Bash
    # This example uses Bash-specific syntax.
    declare -a list=( ... )
    TOOLSET_COMMAND config --menu --arguments "${list[@]}"
    ```

    When used inside external subcommand we can use `edit-file` function
    provided by Command Wrapper Bash subcommand library.

    Key bindings:

    `Enter`
    :   Accept current selection, print it to `stdout`, and exit with status
        code `0`.  If there is no selection to be made, i.e. empty input, then
        pressing `Enter` will result in nothing being printed to `stdout`, and
        terminating with exit status `1`.

    `Up`, `CTRL-P`
    :   Move cursor one line up.

    `Down`, `CTRL-N`
    :   Move cursor one line down.

    `PageUp`
    :   Move cursor one page up.

    `PageDown`
    :   Move cursor one page down.

    `G` (upper-case `g`)
    :   Move cursor to the last line.

    `ESC`, `CTRL-G`, `CTRL-C`
    :   Abort, i.e. nothing is printed to `stdout`, and exit with status code
        `130`.


**Edit File** (`--edit`)
:   Edit configuration file or any other file.  This functionality does allow
    subcommands to respect user editor preferences without needing to
    reimplement editor lookup strategy, or depending on distribution-specific
    tools.

**Initialisation** (`--init`)
:   Initialise toolset configuration.  This action tries to be as safe as
    possible by refusing to overwrite existing files.

    Global Command Wrapper configuration is created when
    `TOOLSET_COMMAND config --init` is invoked under the name `command-wrapper`.
    This can be achieved in one of the following ways:

    ```
    ~/.local/lib/command-wrapper/command-wrapper config --init
    TOOLSET_COMMAND config --init --toolset=command-wrapper
    env COMMAND_WRAPPER_INVOKE_AS=command-wrapper TOOLSET_COMMAND config --init
    ```

    These files and directoreis are created by invoking one of the above:

    ```
    ${XDG_CONFIG_HOME:-$HOME/.config}/command-wrapper/
    │
    ├── default/
    │   ├── aliases-common.dhall
    │   └── help-common.txt
    ├── default.dhall
    │
    ├── cd/
    │   └── directories-common.dhall
    ├── command-wrapper-cd.dhall
    │
    ├── exec/
    │   ├── commands-common.dhall
    │   └── library.dhall
    ├── command-wrapper-exec.dhall
    │
    ├── skel/
    ├── command-wrapper-skel.dhall
    │
    ├── README.md
    └── library.dhall
    ```

    In case of `TOOLSET_COMMAND config --init --toolset=${toolset}` the
    following is created:


    ```
    ${XDG_CONFIG_HOME:-$HOME/.config}/${toolset}/
    │
    ├── default/
    │   ├── aliases-common.dhall
    │   └── help-common.txt
    ├── default.dhall
    │
    ├── cd/
    │   └── directories-common.dhall
    ├── command-wrapper-cd.dhall
    │
    ├── exec/
    │   └──  commands-common.dhall
    ├── command-wrapper-exec.dhall
    │
    ├── skel/
    ├── command-wrapper-skel.dhall
    │
    └── README.md

    $HOME/.local/lib/${toolset}/

    $HOME/.local/bin/ or $HOME/bin/
    ├── ...
    └── ${toolset} -> $HOME/.local/lib/command-wrapper
    ```

    For more information about individual files and directories see
    `command-wrapper(1)`,  `command-wrapper-cd(1)`,  `command-wrapper-exec(1)`,
    and `command-wrapper-skel(1)`.


# OPTIONS

\--dhall
:   Run as interpreter for the Dhall language.

\--\[no-]allow-imports
:   Controls whether imports in the input expression are allowed or not.  By
    default imports are allowed.  Can be specified multiple times, later
    instance is applied.

\--\[no-]alpha
:   Perform α-normalisation of Dhall expression.  By default α-normalisation is
    not performed.  Can be specified multiple times, later instance is applied.

\--\[no-]annotate
:   Add a type annotation to the output.  Type annotations aren't included by
    default.  Can be specified multiple times, later instance is applied.

\--expression=*EXPRESSION*, **\--expression=***EXPRESSION*
:   Use Dhall *EXPRESSION*, as an input instead of reading it from standard input
    or from a *FILE*.  See also **\--input=***FILE*.

\--input=*FILE*, **\--input** *FILE*, **-i** *FILE*
:   Read input from *FILE* instead of standard input.  Can be specified only
    once.  See also **\--expression=***EXPRESSION*.

\--output=*FILE*, **\--output** *FILE*, **-o** *FILE*
:   Write optput into *FILE* instead of standard output.  Can be specified only
    once.

\--\[no-]cache
:   Specifies if caching should be used when resolving imports protected by
    hash.  By default cache is used.

\--let=*NAME*=*EXPRESSION*
:   Declare variable *NAME* with it's value set to *EXPRESSION*, as if it was part
    of the input.  In pseudo Dhall it looks like:

    ```
    let NAME = EXPRESSION
    in  INPUT
    ```

    This is useful if it's not possible to pass Dhall expressions via
    environment variables.

\--dhall-filter
:   Puts Dhall input expression into the scope of *EXPRESSION* as a value
    `input : Input`.

    ```
    $ TOOLSET_COMMAND config --dhall-filter 'input + 1' <<< 1
    2
    $ TOOLSET_COMMAND config --dhall-filter '"num=${Natural/show input}"' <<< 1 | TOOLSET_COMMAND config --dhall-text; echo
    num=1
    ```

\--dhall-format
:   Format Dhall expression.

\--dhall-lint
:   Dhall linter; improve Dhall expression.

\--dhall-resolve
:   Resolve an Dhall expression's imports.

\--list-imports=*KIND*
:   Instead of resolving imports list them one on each line.  *KIND* can be
    *immediate* or *transitive*.

\--dhall-freeze
:   Add integrity checks to import statements of a Dhall expression.

\--\[no-]remote-only
:   Specifies if integrity checks should be added to only remote imports or to
    all imports.  By default they are added only to remote imports.  Can be
    specified multiple times, later instance is applied.

\--for-security, \--for-caching
:   Specifies if integrity checks should be added for the purpose of security
    or caching.  If for caching then alternative import without integrity hash
    is added.  By default we assume that freeze is for security purposes.

\--dhall-hash
:   Compute semantic hashes for Dhall expressions.

\--dhall-diff
:   Render the difference between the normal form of two Dhall expressions.

\--dhall-repl
:   Interpret Dhall expressions in a REPL.

\--init
:   Initialise configuration of a toolset.  This includes symlinking command
    wrapper under the toolset's name.  See also `--toolset=`*NAME* option.

\--toolset=*NAME*
:   When specified allong with `--init` then configuration for toolset *NAME*
    is initialised.  Alternatively `COMMAND_WRAPPER_INVOKE_AS=`*NAME* can be
    used.  See `command-wrapper(1) section *ENVIRONMENT VARIABLES* for details.

\--dhall-bash
:   Compile Dhall expression into Bash expression or statement.

    ```
    $ TOOLSET_COMMAND config --dhall-bash <<< True; echo
    true
    $ TOOLSET_COMMAND config --dhall-bash --decalre=foo <<< True; echo
    declare -r foo=true
    ```

    The `echo` command at the end is to provide newline, since `--dhall-bash`
    doesn't print newline at the end of its output.

\--dhall-text
:   Render Dhall expression as a text.  This allows us to use Dhall as a text
    templating language.

    ```
    $ TOOLSET_COMMAND config --dhall-text --list <<< '"${env:SHELL as Text}\n"'
    /bin/bash
    ```

\--list
:   Render Dhall expression of type **List Text** as lines of text.

    ```
    $ TOOLSET_COMMAND config --dhall-text --list <<< '["foo", "bar"]'
    foo
    bar
    ```

\--declare=*NAME*
:   Compile Dhall expression into a declaration statement, which declares
    variable *NAME*.

    ```
    $ TOOLSET_COMMAND config --dhall-bash --decalre=foo <<< True; echo
    declare -r foo=true
    ```

    The `echo` command at the end is to provide newline, since `--dhall-bash`
    doesn't print newline at the end of its output.

\--dhall-exec
:   Render Dhall expression as Text and execute the result. See also --interpreter=COMMAND.

\--interpreter=*COMMAND*, **\--interpreter** *COMMAND*
:   Run rendered Dhall expression as a script using *COMMAND* as an interpreter.
    See also `--interpreter-argument=`*ARGUMENT*.

\--interpreter-argument=*ARGUMENT*, **\--interpreter-argument** *ARGUMENT*
:   Pass *ARGUMENT* to interpreter *COMMAND*.

\--edit, -e
:   Start editor to edit *FILE* or *SUBCOMMAND* when `--subcommand-config` is
    passed.

\--subcommand-config
:   Open subcommand config when invoked with `--edit`.

\--menu
:   Display selection menu.  Selected value is printed to standard output.

\--arguments \[*STRING* \[...]]
:   Use arguments (*STRING*s) as input.

\--help, -h
:   Display help information and exit.  Same as `TOOLSET_COMMAND help config`.

*EXPRESSION*
:   Dhall expression.

*ARGUMENT*
:   Command line argument passed to executed script in `--dhall-exec` mode.

**TODO: Following options aren't currently implemented!**

\--type, -t
:   Print type of final Dhall expression instead of its value.

\--plain, -p
:   Plain output, final Dhall expression must result in one of:

    * `Text`, `Natural`, or `Integer`
    * `List Text`, `List Natural`, or `List Integer`
    * `Optional Text`, or `Optional Natural`, or `Optional Integer`

\--fail-when-none
:   If result expression is an `Optional` value then it has to be `Some`.

\--fail-when-empty
:   If result expression is a `List` value then it has to be non-empty.

*EXPRESSION*
:   Dhall expression that either queries or updates configuration, depending
    if the `--update` option is present.


# EXIT STATUS

For documentation of generic *EXIT STATUS* codes see `command-wrapper(1)`
manual page section *EXIT STATUS*.  Any *EXIT STATUS* codes specific to this
subcommand will be listed below.

`1`
:   When invoked with `--edit` this status code is returned when no
    match/selection has been made.  This happens when input ends up being empty
    and there is nothing to select.  This is the same status code as `fzf`
    returns under the same conditions.

    This extends the use of `1` as an exit status.  See `command-wrapper(1)`
    manual page to see in what other cases this exit code is returned.

`130`
:   When invoked with `--edit` this status code is returned when selection menu
    was interrupted by **ESC** or **q**.  This is the same status code as `fzf`
    returns in this case.


# FILES AND DIRECTORIES

See also `XDG_CONFIG_HOME` in *ENVIRONMENT VARIABLES* section for more
information on how Command Wrapper figures out where to look for this
configuration file.

`${XDG_CONFIG_HOME:-$HOME/.config}/command-wrapper/user-config/index.dhall`
:   **TODO**

`${XDG_CONFIG_HOME:-$HOME/.config}/command-wrapper/user-config/`
:   **TODO**

`${XDG_CACHE_HOME:-${HOME}/.cache}/${toolset}-dhall-exec/${hash}`
:   Scripts rendered as part of `TOOLSET_COMMAND config --dhall-exec`
    invocation are cached under this paths.  The value of `${hash}` is the
    SHA256 of generated script.


# ENVIRONMENT VARIABLES

See also `command-wrapper(1)` *ENVIRONMENT VARIABLES* section.  Everything
mentioned there applies to this subcommand as well.

`XDG_CONFIG_HOME`
:   Overrides where this subcommand expects its configuration file.  It follows
    this simple logic:

    * If `XDG_CONFIG_HOME` environment variable is set then the configuration
      file has path:

        ```
        ${XDG_CONFIG_HOME}/command-wrapper/user-config/index.dhall
        ```

    * If `XDG_CONFIG_HOME` environment variable is not set then default value
      is used instead:

        ```
        ${HOME}/.config/command-wrapper/user-config/index.dhall
        ```

    See [XDG Base Directory Specification
    ](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html)
    for more information on rationale behind this.


# SEE ALSO

command-wrapper(1), fzf(1)

* [XDG Base Directory Specification
  ](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html)
* [FZF -- A command-line fuzzy finder](https://github.com/junegunn/fzf)
* [FZF -- Related Projects](https://github.com/junegunn/fzf/wiki/Related-projects)
  page includes links to similar tools that could be used instead of FZF.


# BUGS

<https://github.com/trskop/command-wrapper/issues>
