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

## Internal Subcommands

### Completion

*   Expand `completion` to support basic `compgen` actions.  This way we can
    get rid of Bash dependency.

*   Consider having:

    ```
    TOOLSET completion --script [--standalone|--source] ...
    ```

### Config

*   Provide `dhall-json`, `dhall-bash`, and `dhall-text` functionality.

*   Support `EnvironmentVariable` and `List EnvironmentVariable` encoding for
    shell variables.

*   Dhall as an intermediate language:

    ```
    TOOLSET config --dhall --from=INPUT_FORMAT --to=OUTPUT_FORMAT --filter=EXPRESSION
    ```

*   Find a way how we can set history file for `TOOLSET config --dhall-repl`.


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


### Exec

*   Documentation and HOW TOs for `exec` command completion.

*   Evaluate command with and without extra arguments.  If the result is the
    same then print warning to the user.  Dual case would be interesting as
    well.  Having the ability to tell when the command requires additional
    arguments, but there is no obvious simple solution.

*   Support other types of Dhall expression when invoked with
    `--dhall=EXPRESSION`:

    - Already supported: `Verbosity -> ColourOutput -> [Text] -> Command`
    - `Command` -- This would be nice dual to `--print`
    - `NamedCommand`

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

*   Option `--time` to print how long the application took to execute.

*   Command line completion should have access to `Verbosity` and
   `ColourOutput`.  These can affect how `ExecCommand` value is constructed.


### Skell

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

*   Use Haskell Dbus client and desktop notifications library so that we don't
    need `notify-send` to be installed.

*   Find a better way how to play sound when command is done.


## Ideas For New Subcommands

*   Separate repositories for new external subcommands?

*   `tldr` (could have a different name) subcommand like
    <https://github.com/psibi/tldr-hs> but:

    -   Supports XDG Base Directories
    -   Supports mirrors and multiple repositories for tldr pages.
    -   Consider supporting [*Bro pages*](http://bropages.org/) and
        [*eg*](https://github.com/srsudar/eg)