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


## Internal Subcommands

### Completion

*   Expand `completion` to support basic `compgen` actions.  This way we can
    get rid of Bash dependency.

    ```
    TOOLSET completion --query --directories [--regex=REGEX|--glob=GLOB] [PATTERN]
    TOOLSET completion --query --files [--regex=REGEX|--glob=GLOB] [PATTERN]
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

*   Generate and execute wrapper scripts.  This way we can reuse e.g. Bash
    completion script.

    ```
    TOOLSET completion --wrapper
      {--interpreter=COMMAND --interpreter-argument=ARGUMENT [...]|--exec}
      [--expression=EXPRESSION|--input=FILE] [--output=FILE] [[--] ARGUMENTS]
    ```

    Similar to running `dhall-to-text`, writing its output into temporary file,
    and then executing it as a script.

    This may be a functionality that we actually want to have as part of
    `config`'s Dhall functionality.

    **Already partially implemented.**

*   Support fuzzy search when doing `--query`.


### Config

*   Provide `dhall-json`, `dhall-bash`, and `dhall-text` functionality.

*   Support `EnvironmentVariable` and `List EnvironmentVariable` encoding for
    shell variables.

*   Dhall as an intermediate language:

    ```
    TOOLSET config --dhall --from=INPUT_FORMAT --to=OUTPUT_FORMAT --filter=EXPRESSION
    ```

*   Find a way how we can set history file for `TOOLSET config --dhall-repl`.

*   Include editor and shell settings in Command Wrappers global (default)
    config.  Expose them via `config` command to allow scripts and subcommands
    to get access to it.  Also, having something like

    ```
    config --run-editor [FILE [+LINE]]
    ```

    Would be really convenient for multitude of existing use cases.


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

*   Option `--time` to print how long the application took to execute.

*   Command line completion should have access to `Verbosity` and
   `ColourOutput`.  These can affect how `ExecCommand` value is constructed.

*   Use Haskell Dbus client and desktop notifications library so that we don't
    need `notify-send` to be installed.

*   Find a better way how to play sound when command is done.

*   Be able to call `--print` on Dhall expression:

    ```
    TOOLSET exec --expression=DHALL_EXPRESSION [--print] [--] [ARGUMENTS]
    ```


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
