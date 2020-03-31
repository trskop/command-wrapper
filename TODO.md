# Ideas and TODOs

## Command Wrapper

*   (**IN PROGRESS**) **Support for installing via Nix**:

    -   (**DONE**) How to set system config directory in Nix?  (See also **Fallback
        configuration files for subcommands.**)  There are few ideas:

        *   Introduce environment variable `COMMAND_WRAPPER_SYSTEM_CONFIG_DIR`
            for this purpose.

    -   (**DONE**) Allow using facade (Nix-style wrapper script) for Command
        Wrapper.

        Introduces `COMMAND_WRAPPER_FACADE` environment variable that specifies
        what executable to use instead of resolving underlying Command Wrapper
        executable.

    -   Variation of toolset initialisation for Nix:

        ```
        TOOLSET config --init --toolset=NAME --nix
        ```

        Or:

        ```
        TOOLSET config --nix --toolset=NAME
        ```

        Which would create a Nix expression to install the toolset.  It could
        bootstrap the whole directory structure for the toolset as well.

    -   Initialisation of toolset must be aware of Nix.  When we call:

        ```
        TOOLSET config --init --toolset=NAME
        ```

        And `command-wrapper` exuecutable is installed via Nix, then symbolic
        link will be broken whenever we update Nix packages.  Instead, we
        should generate a script that understands how to call command wrapper,
        or point to `~/.nix-profile/libexec/command-wrapper` instead of
        executable directly.

    -   (**DONE**) Completion script that works in Nix with wrapper scripts:

        ```
        TOOLSET completion --script [--shell=SHELL] [--toolset=NAME]
          [--executable=PATH] [--output=FILE] [--alias=ALIAS ...]
        ```

        Where `--executable=PATH` allows us to specify wrapper to be called and
        `--toolset=NAME` allows us to override inferred toolset name.

*   **Config file control.**

    Since configuration now has three levels (system, user, and local) we can
    provide command line option to override the local one, which otherwise uses
    `COMMAND_WRAPPER_LOCAL_CONFIG_DIR` environment variable.

    ```
    TOOLSET --local-config-dir[ectory]=DIRECTORY SUBCOMMAND [OPTIONS]
    ```

*   **Fallback configuration files for subcommands.**  If there is no configuration
    file for a subcommand inside toolset-specific config directory, then these
    would be used.

    This is not as easy as it sounds.  First step in this direction was
    changing Subcommand Protocol to specify that `COMMAND_WRAPPER_CONFIG`
    contains Dhall expression and not a file path.  This way Command Wrapper
    has full control over file resolution and potential composition of
    configuration.  However, the ideas on how to do the resolution and
    composition are not completely solid.

    Initial step for changing the config file resolution (finding which config
    file to use) has been made by supporting three levels of configuration for
    toolset:

    *   System level configuration.
    *   User level configuration.
    *   Local AKA project-level configuration.

    This may be related to parametrisation of `main` function.  That way we
    could avoid hardcoding paths all over the place.

*   **Pipe help message to pager** if on terminal and the message is too long.

*   **Configuration for colours**:

    -   Messages: warning, error, info, etc.
    -   Help message colours: option, metavariable, command, etc.

    This will require Subcommand Protocol to be extended.

*   (**DONE**) **Better security**:

    -   (**DONE**) Extend Dhall subcommands to have `--secure-remote-imports`, or
        similar, where URL imports would be required to be guarded by integrity
        hash.

    -   (**DONE**) Not search `PATH` for subcommands.

*   (**IN PROGRESS**) **Configurable/extensible `main`**:

    -   (**DONE**) Move toolset `main` function into `CommandWrapper.Toolset.Main`.


    -   Parametrise toolset `main` function with options that specify how it
        should behave.

    -   Pass at least following parameter: toolset version so that it doesn't
        have to be the same as `command-wrapper` library.

*   (**IN PROGRESS**) **Restructure library into three**:

    -   `command-wrapper-core` that uses `CommandWrapper.Core` module prefix.
    -   `command-wrapper-subcommand` that uses `CommandWrapper.Subcommand`
        module prefix.
    -   `command-wrapper` that uses `CommandWrapper.Toolset` module
        prefix.

    Steps:

    1.  (**DONE**) Reorganise modules so that they fit into the
        hierarchy mentioned above:

        -   CommandWrapper.Core
        -   CommandWrapper.Subcommand
        -   CommandWrapper.Toolset

        Some modules will have to be split, moved, reorganised to better fit
        the above structure.

    2.  (**DONE**) Split them into following packages where

        -   `command-wrapper-core` that contains all `CommandWrapper.Core`
            modules.

        -   `command-wrapper-toolset` that contains all `CommandWrapper.Toolset`
            modules and `command-wrapper` executable (`app/Main.hs`).

        -   `command-wrapper-subcommand` that contains all
            `CommandWrapper.Subcommand` modules.

    3.  Figure out where to put `cd`, `skel` and `exec` subcommands.

        While they are very useful it may not make sense to put them into
        `command-wrapper` or `command-wrapper-subcommand`.  They may
        need a package each.  It may be the most modular approach as well.

        This may require moving some submodules from
        `CommandWrapper.Toolset.Config` into
        `CommandWrapper.{Core,Subcommand}.Config`.  Otherwise they (`cd`,
        `skel`, `exec`) would have to depend on toolset library.

*   (**IN PROGRESS**) **Direnv support**:

    1.  (**DONE**) Introduce a library that extends Direnv's stdlib:

        ```
        TOOLSET completion --library --direnv [--content|--import]
            [-o FILE|--output=FILE]
        ```

        This may be an alternative if we'll need more Direnv-related libraries:

        ```
        TOOLSET completion --library --direnv=envrc [--content|--import] \
            [-o FILE|--output=FILE]
        ```

    2.  Introduce:

        ```
        TOOLSET completion --query {--man-path}
        ```

        So that we can add them to `MANPATH` in `.envrc`.

    3.  Introduce:

        ```
        TOOLSET config --init --direnv
        ```


## Internal Subcommands

### Completion

*   **Extend file system completion to support regular expresions and globs**:

    -   Regular expressions and glob patterns to filter vialble completions:

        ```
        TOOLSET completion --query --file-system=TYPE [--regex=REGEX|--glob=GLOB] [--pattern=PATTERN]
        ```

    -   Support `--algorithm=ALGORITHM` option.  At the moment only prefix
        equivalence is used.

    -   Consider switching from `--file-system=TYPE` to
        `--file-system [--type=TYPE]`.  When `--type=TYPE` is omitted then
        entry type is completely ignored.

*   **Hosts completion**:

    ```
    TOOLSET completion --query --hosts --input=FORMAT:FILE [...] [PATTERN]
    ```

    Where `FORMAT` is one of `hosts`, `known_hosts`, or `ssh_config`.  By
    default it should behave as if executed as:

*   **Consider having variations of completion script**:

    ```
    TOOLSET completion --script [--standalone|--source] ...
    ```

*   **Support for custom libraries shell and Dhall libraries**:

    ```
    TOOLSET completion --library --custom=NAME [--import|--content] [--output=FILE]
    ```

    Maybe this will require a little more thought.

*   Implement `--strip-prefix` option that would strip `STRING` (specified via
    `--prefix=STRING`) from `PATTERN` (specified via `--pattern=PATTERN`).

    ```
    TOOLSET completion --query QUERY_WHAT_OPTION
      [--algorithm=ALGORITHM]
      [--pattern=PATTERN]
      [--prefix=STRING [--strip-prefix]]
      [--suffix=STRING]
      [--output=FILE]
    ```

*   If we start thinking of command line completion as a finite-state machine,
    and we are able to encode states and transition function in Dhall, then
    following tool could get such description as an input:

    ```
    TOOLSET completion --compgen
        [--input=FILE|--expression=EXPRESSION]
        [--index=NUM]
        [--shell=SHELL]
        -- [WORD ...]
    ```

    Most of the ideas above this one would become primitives in the completion
    description DSL.


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

    * Consider:

        ```
        config --edit --{local,user,system}-config
        ```

*   Enhancements to `--menu`:

    *   Extend global config file to allow specify alternative implementation
        of `--menu` functionality.  Like `fzf`, `fzy`, `skim`, etc.  This would
        allow users to have consistent feel with their outher tools and
        integrations.

    *   Introduce variation that takes Dhall expression of the form

        ```Dhall
        List { mapKey : Text, mapValue : v }
        ```

        The value of `mapKey` will be displayed in the selection menu, and
        `mapValue` will be printed if the item is selected.  Value printed to
        the standard output will be a Dhall expression of the type `v`.

*   Pager mode:

    ```
    TOOLSET config --pager [--ansi] [--input=FILE]
    ```

*   Provide `--plain` option for Dhall interpreter:

    ```
    \--plain, -p
    :   Plain output, final Dhall expression must result in one of:

        * `Text`, `Natural`, or `Integer`
        * `List Text`, `List Natural`, or `List Integer`
        * `Optional Text`, or `Optional Natural`, or `Optional Integer`
    ```


### Help

*   Implement `TOOLSET help --list` that would print subcommands and short help
    message.  It should include aliases, but it should be obvious what they
    are.

*   `TOOLSET help --man` needs to understand aliases.

*   (**DONE**) Completion should support topics as well.

*   All `command-wrapper-${TOPIC}` and `${TOOLSET}-${TOPIC}` manual pages
    should be accessible.  This doesn't have to be by understanding how `man`
    searches for pages, but it can be implemented as part of toolset
    configuration.

*   We should be able to default to `command-wrapper(1)` when there is no
    toolset-specific manual page.

*   `TOOLSET help --format [--input=FILE] [--subcommand=NAME]` that takes Dhall
    representation of help message and prints it formatted including colours.

*   (**DONE**) Be able to access manual pages inside `nix-shell` correctly.
    Resolved by using separate man path.


## Bundled External Subcommands

### Cd

-   Support for glob patterns in configuration?  Would be useful for something
    like `~/Devel/*`, which would list all the immediate subdirectories of
    `~/Devel`.

-   Option to detach terminal emulator process.  (Spawn a new process, and let
    the parent die.  Make sure that stdin/stdout are detached, and killing
    original process where `TOOLSET cd` was invoked won't kill the terminal.)

-   Option to print a command that would be performed.  This is useful for
    shell key-bindings.

-   Support directory specific commands.  For example we may want a specific
    shell for a certain directory.

-   Support Neovim remote to open a new terminal buffer.  In other words, if we
    have `nvr` (neovim-remote) installed then we can open a terminal buffer
    from within a Vim/Neovim terminal as well as any other terminal if we pass
    Neovim listening socket/address to it.  See `yx jmp` for more information.


### Exec

*   Documentation and HOW TOs for `exec` command completion.

*   Evaluate command with and without extra arguments.  If the result is the
    same then print warning to the user.  Dual case would be interesting as
    well.  Having the ability to tell when the command requires additional
    arguments, but there is no obvious simple solution.

*   (**IN PROGRESS**) Support other types of Dhall expression when invoked with
    `--expression=EXPRESSION`:

    -   (**DONE**) `Verbosity -> ColourOutput -> [Text] -> ExecCommand`
    -   (**DONE**) `ExecCommand` -- This would be nice dual to `--print`
    -   (**DONE**) `Command` -- Same as `ExecCommand`, but without
        `workingDirectory : Optional Text` and `searchPath : Bool` fields.
    -   (**DONE**) `ExecNamedCommand`  -- Best for testing.

    Additional features:

    -   Completion works in case of using `ExecNamedCommand`.
    -   Better error message if we fail to deserialise any supported format,
        i.e. type.

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

*   `askpass` -- Something that allows us to abstract away from `ssh-askpass`
    variants and maybe provides a sensible (minimalistinc) default.  It should
    have the capability of working on terminal as well.
