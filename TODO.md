# Ideas and TODOs

## Command Wrapper

*   Support for installing via Nix:

    -   Override for config dir? `COMMAND_WRAPPER_CONFIG_DIR`, or
        `NIX_COMMAND_WRAPPER_ROOT` environment variable?  How can we mix this
        with user configuration?

*   Config file control:

    ```
    TOOLSET --config=FILE --subcommand-config=FILE SUBCOMMAND [OPTIONS]
    ```

*   Fallback configuration files for subcommands.  If there is no configuration
    file for a subcommand inside toolset-specific config directory, then these
    would be used.

*   Pipe help message to pager if on terminal and the message is too long.

*   Configuration for colours:

    -   Messages: warning, error, info, etc.
    -   Help message colours: option, metavariable, command, etc.

    This will require Subcommand Protocol to be extended.


## Internal Subcommands

### Completion

*   Expand `completion` to support basic `compgen` actions.  This way we can
    get rid of Bash dependency.

    ```
    TOOLSET completion --query --directories [--regex=REGEX|--glob=GLOB] [--pattern=PATTERN]
    TOOLSET completion --query --files [--regex=REGEX|--glob=GLOB] [--pattern=PATTERN]
    ```

*   Hosts completion:

    ```
    TOOLSET completion --query --hosts --input=FORMAT:FILE [...] [PATTERN]
    ```

    Where `FORMAT` is one of `hosts`, `known_hosts`, or `ssh_config`.  By
    default it should behave as if executed as:

*   Consider having:

    ```
    TOOLSET completion --script [--standalone|--source] ...
    ```

*   Support fuzzy search when doing `--query`.

*   Support for custom libraries:

    ```
    TOOLSET completion --library [--shell=SHELL|--custom] [--import|--content] [--output=FILE]
    ```

    Maybe this will require a little more thought.

*   Introduce `--prefix=STRING` and `--suffix=STRING` options that are added to
    results.


### Config

*   Provide `dhall-json` functionality.

*   Support `EnvironmentVariable` and `List EnvironmentVariable` encoding for
    shell variables.

*   Dhall as an intermediate language:

    ```
    TOOLSET config --dhall --from=INPUT_FORMAT --to=OUTPUT_FORMAT --filter=EXPRESSION
    ```

*   Find a way how we can set history file for `TOOLSET config --dhall-repl`.

*   Support most of options of `--dhall` mode in `--dhall-filter` mode as well.

*   Following Dhall commands should have `--let=VARIABLE=EXPRESSION` option:

    ```
    TOOLSET config --dhall-text [OPTIONS]
    TOOLSET config --dhall-bash [OPTIONS]
    ```

*   Command line completion for `EXPRESSION` in `--let=VARIABLE=EXPRESSION`.

*   `--dhall-list` functionality that takes a list as an input and produces
    Dhall list as an output:

    ```
    $ printf 'foo\nbar\n' | "${TOOLSET}" config --dhall-list --as=Text
    [ "foo", "bar" ]
    ```

    ```
    TOOLSET config --dhall-list
      [--nul[l]|-0|--new-line|--spaces|--delimiter=DELIMITER]
      [--as=FORMAT]
    ```

    Where:

    -   `--nul[l]|-0` uses `\0` (NUL character) as a delimiter.
    -   `--spaces` uses sequence of on or more spaces as a delimiter.
    -   `--new-line` uses end-of-line delimiter.  This can be LF, CRLF, or CR.
    -   `DELIMITER` is a single character deriliter.
    -   `FORMAT` is `Text`, in the future it will make sense to have something
        as `JSON t` where `t` is a Dhall type.

    We should be able to do dual as well, i.e. convert Dhall value of type
    `List Text` into delimiter separated text.  This may be interesting when
    used with `xargs`.  However, there's an overlap with `--dhall-text` as well
    as `--dhall-filter`.

*   Include editor and shell settings in Command Wrappers global (default)
    config.  Expose them via `config` command to allow scripts and subcommands
    to get access to it.  Editor is already exposed via `config` subcommand,
    but improvements would be nice.  That includes:

    *   Extend global configuration file to specify default editor to use.

    *   Option to allow override of default editor:
        `--override-default={COMMAND|EXPRESSION}`.  Where `COMMAND` is just a
        command name and `EXPRESSION` is a Dhall expression for composing
        command for editing a file.  It may be better idea to have two options
        instead of one that analyses its pargument, e.g.
        `--override-default-command=COMMAND` and
        `--override-default-expression=EXPRESSION`.

    *   Allow specifying line number:

        ```
        config --edit [FILE [+LINE]|--subcommand-config SUBCOMMAND [+LINE]]
        config --edit [FILE [--line=LINE]|--subcommand-config SUBCOMMAND [--line=LINE]]
        ```

        Not sure if this will be reliable enough to be actually useful.

    *   Option that defines an environment variable name
        (`--override-variable=NAME`) that would have precedence over `VISUAL`,
        `EDITOR`, and default editor.  This could be used by subcommands to
        allow users to override editor specifically for that subcommand.


### Help

*   `TOOLSET help --man` needs to understand aliases.

*   Completion should support topics as well.

*   All `command-wrapper-${TOPIC}` and `${TOOLSET}-${TOPIC}` manual pages
    should be accessible.

*   We should be able to default to `command-wrapper(1)` when there is no
    toolset-specific manual page.

*   `TOOLSET help --format [--input=FILE] [--subcommand=NAME]` that takes Dhall
    representation of help message and prints it formatted including colours.

*   Be able to access manual pages inside `nix-shell` correctly.


## Bundled External Subcommands

### Cd

-   Support for glob patterns in configuration?  Would be useful for something
    like `~/Devel/*`, which would list all the immediate subdirectories of
    `~/Devel`.

-   Option to detach terminal emulator process.  (Spawn a new process, and let
    the parent die.  Make sure that stdin/stdout are detached, and killing
    original process where `TOOLSET cd` was invoked won't kill the terminal.)

-   Option to print a command that would be performed.  This is useful for
    shell key-bindings.  Printing plain `cd ${dir}` would be nice as well.

-   Support directory specific commands.  For example we may want a specific
    shell for a certain directory.

-   Support Neovim remote.  See `yx jmp` for more information.

-   Make it aware of Vim/Neovim terminal, which can prevent us to create new
    terminal window.

-   Provide default menu tool implementation so that command can work without
    `fzf`/`fzy` installed.


### Exec

*   Documentation and HOW TOs for `exec` command completion.

*   Evaluate command with and without extra arguments.  If the result is the
    same then print warning to the user.  Dual case would be interesting as
    well.  Having the ability to tell when the command requires additional
    arguments, but there is no obvious simple solution.

*   Support other types of Dhall expression when invoked with
    `--expression=EXPRESSION`:

    - Already supported: `Verbosity -> ColourOutput -> [Text] -> ExecCommand`
    - `ExecCommand` -- This would be nice dual to `--print`
    - `ExecNamedCommand`  -- Should completion work in this case?  Probably yes.

    We may want to consider having:

    ```
    TOOLSET exec --commands=DHALL_EXPRESSION [--] COMMAND [COMMAND_ARGUMENTS]
    ```

    Where `DHALL_EXPRESSION` has type `List ExecNamedCommand`.  This would be
    very useful for trying new stuff and debugging without having to change
    `exec`'s config file.  Command line completion should work as well for this.

*   Configuration for desktop notifications:

    ```
    { commands : List ExecNamedCommand
    , notifications :
        { when : Optional NotifyWhen
        , icon : Optional (∀(exitCode : ExitCode) → Optional NotifyIcon)
        , urgency : Optional (∀(exitCode : ExitCode) → Optional NotifyUrgency)
        , message : Optional (∀(exitCode : ExitCode) → ∀(msg : Text) → Text)
        , soundFile : Optional Text
        , forceSound : Optional Bool
        }
    , ...
    }
    ```

*   Option `--time` to print how long the application took to execute.  The
    information should be printed directly to the terminal, and if there is no
    controlling terminal it should be ignored.  We should consider adding
    `--time-output=FILE` option as well.

*   Command line completion should have access to `Verbosity` and
   `ColourOutput`.  These can affect how `ExecCommand` value is constructed.

*   Use Haskell Dbus client and desktop notifications library so that we don't
    need `notify-send` to be installed.

*   Find a better way how to play sound when command is done.

*   Be able to call `--print` on Dhall expression:

    ```
    TOOLSET exec --expression=DHALL_EXPRESSION [--print] [--] [ARGUMENTS]
    ```

*   Introduce `--tee=FILE [--tee-append]` to store stdout in `FILE` as well as
    printing it on command line.

    ```
    [--input=FILE] [--output=FILE|--tee=FILE [--tee-append]]
    ```

*   Introduce `--pager` to pipe output to a pager.  Should also:

    -   Respect `--colour=always`, i.e. pass `-R` to less, etc.  Different pager
        commands based on `--colour=always`?
    -   Work with `--tee=FILE [--tee-append]`.


### Skel

*   Generalise `skel` command by merging in `yx new` functionality.

*   Skeletons of configuration files would be very useful.  Not only an empty
    one for the newly created subcommand, but also being able to have various
    commonly used configuration skeletons.  Or is that something that `config`
    subcommand should be doing?

*   When `TOOLSET skel SUBCOMMAND --language=dhall` is invoked we could try to
    find a specific skeleton for that subcommand.

*   Maybe switch syntax of config skeletons to:

    ```
    TOOLSET skel {--config|-c} {--toolset|SUBCOMMAND}
    ```

*   Something like `--edit-only` to edit already created file?  How would this
    play wity `yx new` functionality?  Two kind of templates?


## Ideas For New Subcommands

*   Separate repositories for new external subcommands?

*   `tldr` (could have a different name) subcommand like
    <https://github.com/psibi/tldr-hs> but:

    -   Supports XDG Base Directories
    -   Supports mirrors and multiple repositories for tldr pages.
    -   Consider supporting [*Bro pages*](http://bropages.org/) and
        [*eg*](https://github.com/srsudar/eg)
