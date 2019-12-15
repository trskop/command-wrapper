% COMMAND-WRAPPER-SUBCOMMAND-PROTOCOL(7) Subcommand Protocol | v1.0.0
% Peter Trsko
% 15th December 2019


# NAME

Protocol that must be respected by all external *Command Wrapper* subcommands.


# DESCRIPTION

Command Wrapper follows a specific calling convention for external subcommands.
Subcommands are required to follow a specific protocol when invoked, which is
described in this manual page.


# COMMAND LINE ARGUMENTS

Each subcommand must support following options:

`--help`, `-h`
:   Prints subcommand specific help message.  Help message must be printed to
    standard output, and command must terminate with exit code 0.  This option
    is invoked by Command Wrapper's `help` internal subcommand.

    While having short option `-h` is not strictly necessary, this protocol
    mandates that it should be provided.  This way subcommands will have
    consistent UI with Command Wrapper, and there are no surprises in the form
    of `-h` being used for other purposes.

`--completion-info`
:   Prints a Dhall expression describing how subcommand should be called to
    provide command line completion.  Command must terminate with exit code 0.
    See **COMMAND LINE COMPLETION** section for more details.

    This command should not be described in the help message printed when the
    subcommand is invoked with `--help`.  It is considered internal option, and
    hiding it allows us to evolve Subcommand Protocol without affecting
    user-facing UI.


# ENVIRONMENT VARIABLES

When external subcommand is executed then the following environment variables
are available to it:

`COMMAND_WRAPPER_EXE`
:   Contains full path to Command Wrapper executable.  Usually
    `${HOME}/.local/lib/command-wrapper/command-wrapper`, e.g.
    `/home/joe/.local/lib/command-wrapper/command-wrapper`.

`COMMAND_WRAPPER_VERSION`
:   Version of subcommand protocol that Command Wrapper expects the subcommand
    to respect.

`COMMAND_WRAPPER_NAME`
:   Contains name under which Command Wrapper was executed.  This is not a file
    path, just command name.

    For example if we run:

    ```
    toolset foo
    ```

    Then we get:

    ```
    COMMAND_WRAPPER_NAME=toolset
    ```

`COMMAND_WRAPPER_SUBCOMMAND`
:   Contains name of the subcommand that is being executed from the perspective
    of Command Wrapper.  This is not a file path.

    For example if we run:

    ```
    toolset foo
    ```

    Then we get:

    ```
    COMMAND_WRAPPER_SUBCOMMAND=foo
    ```

`COMMAND_WRAPPER_CONFIG`

:   Contains a file path to subcommand configuration file. Subcommand may
    ignore it entirely, but if subcommand is using it then it has to correctly
    handle the case when the configuration file doesn't exist.  It's up to
    individual subcommand to decide which of these scenarios it will use if the
    configuration doesn't exist:

    1.  Use hard-coded defaults.  Subcommand may generate the configuration
        file with these defaults.  If it does so then the user must be notified
        by a message that it was done so. Such message is subject to verbosity
        setting (see `COMMAND_WRAPPER_VERBOSITY`), i.e. if verbosity is set to
        `silent` then the message is not actually printed.

    2.  Fail with error message indicating that the configuration file is
        missing.

    See also **CONFIGURATION FILE** section.

`COMMAND_WRAPPER_VERBOSITY`
:   Contains one of:

    *   `silent` -- Don't print any messages, not even error messages.
    *   `normal` -- Print only important messages.
    *   `verbose` -- Print anything that comes into mind.
    *   `annoying` -- Print debugging/tracing information.

    Subcommand must respect these values if it's producing any output that is
    not part of user interaction.  Note that error messages should be supressed
    in `silent` mode, and only *EXIT STATUS* should be an indication of error.

    Value of *VERBOSITY* should not affect output that is a part of interactive
    session with the user.

`COMMAND_WRAPPER_COLOUR`
:   Contains one of:

    *   `always` -- Always use colourised output, even if output is not a
        terminal.  This can be useful when, for example, piping output to a
        pager.
    *   `auto` -- Use colourised output when the output is a terminal that
        supports it.
    *   `no` -- Never use colourised output.

    Subcommands aren't requred to support colourised output, but if they do
    then they have to respect this environment variable.


# CONFIGURATION FILE

Subcommand may require a configuration file. If it does require one then it
must use the one provided in `COMMAND_WRAPPER_CONFIG`, which is always in
[Dhall](https://github.com/dhall-lang/dhall-lang#readme) format. Subcommand
must not use any other configuration file, with the notable exception of it's
dependencies, e.g. DNS resolution uses `/etc/resolv.conf`.  However, if it's
possible then temporary configuration file should be generated and passed to
the dependency explicitly.

When this is respected by the subcommand then Command Wrapper has full control
over subcommand configuration and command line arguments that are passed to it.
This way it can guarantee consistent UI.

For purpose of reading Dhall configuration files in Bash, Command Wrapper
provides bunch of Dhall-related commands as part of its internal subcommand
`config`.  For more information see `command-wrapper-config(1)` manual page or:

```
TOOLSET_COMMAND help [--man] config
```


# EXIT STATUS

Some of the *EXIT STATUS* codes were inspired by Bash exit codes.  See e.g.
[Advanced Bash-Scripting Guide: Appendix E. Exit Codes With Special Meanings
](http://tldp.org/LDP/abs/html/exitcodes.html) for reference.

`0`
:   If everything went OK.

    Notably this exit status is returned if subcommand is invoked with `--help`
    or `--completion-info` option and there was no command line options parsing
    error.  See also **COMMAND LINE ARGUMENTS** section.

`1`
:   If subcommand has encounter one of the following issues:

        * Unable to parse command line arguments/options.
        * Unable to parse configuration file, or if there was a type error in
          a configuration file.
        * Required configuration file is missing.

`2`
:   Returned by a subcommand if the Command Wrapper environment wasn't set up
    properly.  In other words Subcommand Protocol was violated by the caller.

    This can indicate a version mismatch between Command Wrapper installation,
    and subcommand/toolset installation.

`255`
:   Exit status out of range.  Some subroutine tried to terminate with exit
    code that is out of 0-255 range.

*OTHER*
:   Subcommands are free to use other *EXIT STATUS* codes if there is no
    *EXIT STATUS* defined for that purpose.


# COMMAND LINE COMPLETION

Subcommand has to support `--completion-info` command line option which causes
it to print to standard output a Dhall expression.  That expression describes
how it has to be called to provide command line completion.  Type signature of
mentioned Dhall expression is:

```Dhall
  < Bash | Fish | Zsh  >  -- Shell under which completion was invoked.

→ Natural  -- Index of a word that is beeing completed.

→ List Text  -- List of words as they are mentioned on current command line.
             --   Only those deemed to be passed to subcommand are present.

→ List Text  -- Command line arguments with which the subcommand should be
             --   executed for the completion to work.
```

Algorithm:

*   Command line completion is invoked on Command Wrapper toolset, which parses
    current command line.  If it is able to respond it does so.

*   If Command Wrapper needs to complete arguments for a subcommand it calls
    the subcommand as:

    ```
    "/some/path/${COMMAND_WRAPPER_NAME}-${COMMAND_WRAPPER_SUBCOMMAND}" --completion-info
    ```

    Which will produce a Dhall functintion which is then evaluated into a new
    command line that should be called:

    ```
    "/some/path/${COMMAND_WRAPPER_NAME}-${COMMAND_WRAPPER_SUBCOMMAND}" "${RESULT_OF_APPLIED_DHALL_EXPRESSION[@]}"
    ```

*   Subcommand provides list of possible completions which are returned to the
    shell.

Standard Command Wrapper command line completion API uses following syntax:

```
SOME_COMMAND --completion --index=INDEX --shell={bash|fish|zsh} -- [WORD [...]]
```

This transcribes into following Dhall expression to be returned as a result of
`--completion-info`:

```Dhall
  λ(shell : < Bash | Fish | Zsh >)
→ λ(index : Natural)
→ λ(words : List Text)
→   [ "--completion"
    , "--index=${Natural/show index}"
    , "--shell=${merge {Bash = "bash", Fish = "fish", Zsh = "zsh"} shell}"
    , "--"
    ]
  # words
```

It is advised to use it whenever there is no need to use something else.
Example of a case when other style may be required is if command line option
parsing library has completion support embedded.  In which case the embedded
one is used.

Command Wrapper's Bash library provides `stdCompletionInfo` that provides the
above Dhall expression.  For more information see documentation embedded in the
library itself:

```
TOOLSET_COMMAND completion --library --shell=bash --content | less
```

Command line completion API between shell and Command Wrapper is provided by
`completion` subcommand.  It also provides some useful utilities that may be
used from subcommands, especially when written in shell scripting language.

For more information see also `command-wrapper-completion(1)` manual page or:

```
TOOLSET_COMMAND help [--man] completion
```


# SEE ALSO

command-wrapper(1), command-wrapper-bash-library(7),
command-wrapper-completion(1), command-wrapper-config(1)

* [Dhall configuration language](https://dhall-lang.org)
* [Advanced Bash-Scripting Guide: Appendix E. Exit Codes With Special Meanings
  ](http://tldp.org/LDP/abs/html/exitcodes.html)
