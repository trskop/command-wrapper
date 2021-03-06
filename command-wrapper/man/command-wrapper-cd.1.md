% COMMAND-WRAPPER-CD(1) Command Wrapper 0.1.0 | Command Wrapper
% Peter Trsko
% 1st September 2020


# NAME

`command-wrapper-cd` - Start a new subshell / Tmux window / terminal emulator
in a selected directory.


# USAGE

TOOLSET\_COMMAND \[GLOBAL\_OPTIONS] cd
\[\--shell|-s|\--tmux|-t|\--kitty|-k|\--terminal|-e]
\[\--query=QUERY|-q *QUERY*]
\[\[\--] *DIRECTORY*]

TOOLSET\_COMMAND \[GLOBAL\_OPTIONS] cd
{\--bash-command|\--sh-command}
\[\--query=QUERY|-q *QUERY*]
\[\[\--] *DIRECTORY*]

TOOLSET\_COMMAND \[GLOBAL\_OPTIONS] cd \--self-command
\[\--shell|-s|\--tmux|-t|\--kitty|-k|\--terminal|-e|\--{bash|sh}-command]
\[\--query=QUERY|-q *QUERY*]
\[\[\--] *DIRECTORY*]

TOOLSET\_COMMAND \[GLOBAL\_OPTIONS] cd {\--help|-h}

TOOLSET\_COMMAND \[GLOBAL\_OPTIONS] help cd


# DESCRIPTION

It is common that while working on something one needs to jump into another
directory, do something there, and then go back.  This command tries to solve
this by spawning a subshell, or a new Tmux/Kitty window, or a terminal
emulator, see *OPTIONS* for details.

Behaviour of this command is similar to `cd` shell command, but allows the user
to select a directory from a pre-defined list.  This speeds up access to
commonly accessed directories.

When no option is specified, `cd` subcommand checks if it's running in Tmux or
Kitty, and if it is then it opens a new Tmux or Kitty window, respectively.  If
it's not executed inside Tmux session or Kitty terminal then it executes a new
instance of `$SHELL` in a selected directory.

Selecting directory from a list is implemented using external command.  Default
configuration uses:

```
TOOLSET_COMMAND --no-aliases config --menu
```

Dhall library provides smart constructors for two other tools that are known
to work with `cd` subcommand:

* [`fzf`](https://github.com/junegunn/fzf)
* [`fzy`](https://github.com/jhawthorn/fzy)
* [`sk` (Skim)](https://github.com/lotabout/skim)

For more information consult Command Wrapper Dhall library.


# OPTIONS

\--shell, -s
:   Execute a subshell even if in a Tmux session or in Kitty terminal.

\--tmux, -t
:   Create a new Tmux window, or fail if not in Tmux.

\--kitty, -k
:   Create a new Kitty window, or fail if not in Kitty.

\--terminal, -e
:   Open a new terminal emulator window.

\--bash-command
:   Print a command to change directory.  If escaping is required then Bash
    string literal is produced. Useful for Bash (obviously), Zsh, and other
    shells that support string literals using `$'...'` syntax.

\--sh-command
:   Print a command to change directory. If escaping is required then Bourne
    Shell escaping is used. Useful for shells like Dash.

\--self-command
:   Print a command to execute this subcommand with selected directory inlined.
    Very useful for shell shortcut bindings.

\--query=*QUERY*, \--query *QUERY*, -q *QUERY*
:  Start the search for a directory with the given *QUERY*.  This option may be
   ignored if the underlying menu tool doesn't support this functionality.  FZF
   does support it, see FZF documentation of `--query` option.

\--help, -h
:   Display help information and exit.  Same as `TOOLSET_COMMAND help cd`.

*DIRECTORY*
:   Use *DIRECTORY* instead of asking user to select one from a list.

    In other words, start a new shell, open a Tmux window, new Kitty window, or
    print a command for changing directory to *DIRECTORY* instead of requesting
    user to select one.  The value of *DIRECTORY* doesn't have to be a
    directory preconfigured in configuration file.


# EXIT STATUS

For documentation of generic *EXIT STATUS* codes see `command-wrapper(1)`
manual page section *EXIT STATUS*.  Specific *EXIT STATUS* codes to this subcommand
are listed below.

`3`
:  Command was unable to find target directory or it was unable to determine
   what shell to execute.  The later usually means that `SHELL` environment
   variable is not present in the current environment.


# BASH KEY BINDINGS

Example of binding `TOOLSET_COMMAND cd` to CTRL-f:

```Bash
bind '"\C-f":"TOOLSET_COMMAND cd\n"'
```

Please change `TOOLSET_COMMAND` to whatever you are using, or use full path to
`command-wrapper` executable.  Usually:

```Bash
${HOME}/.local/lib/command-wrapper/command-wrapper
```

## ADVANCED BASH KEY BINDINGS

This is based on `fzf` bindings. They avoid using `\e` (escape) to avoid delays
in terminal waiting for the next key. To get the same behaviour they bound
`\C-x\C-a` (`CTRL-x CTRL-a`). Using the same bindings as `fzf` allows us to
limit number of bindings that are in use, however, it also has a downside if
`fzf` bindings decide to change something and we interfere.

In the addition to the above `C-m` (`CTRL-m`) is used in the bindings below and
it's just standard vi binding for `<CR>` AKA new line.

Don't forget to expand `${TOOLSET}`, it won't work otherwise.

```Bash
bind '"\C-x\C-a":vi-movement-mode'
bind '"\C-x\C-e":shell-expand-line'
bind '"\C-x\C-r":redraw-current-line'
bind '"\C-f":"`${TOOLSET} cd --self-command --shell`\C-x\C-e\C-x\C-r\C-m"'
bind '"\C-\M-f":"`${TOOLSET} cd --bash-command`\C-x\C-e\C-x\C-r\C-m"'
bind '"\C-k":"`${TOOLSET} cd --self-command`\C-x\C-e\C-x\C-r\C-m"'
```

When `CTRL+f` or `CTRL-ALT-f` is pressed in normal mode then switch to insert
mode and call it there:

```Bash
bind -m vi-command '"\C-f":"i\C-f"'
bind -m vi-command '"\C-\M-f":"i\C-\M-f"'
```

Following is the summary and description of keybindings introduced above:

`CTRL+f`
:   Calls and expands:

    ```Bash
    TOOLSET cd --self-command --shell
    ```

    Into following command which ends up being executed and stored in Bash
    history:

    ```Bash
    TOOLSET cd --shell DIRECTORY
    ```

    Where `DIRECTORY` is what user selected during expansion of the original
    command.

`CTRL+ALT+f`
:   Calls and expands

    ```Bash
    TOOLSET cd --bash-command
    ```

    Into following command which ends up being executed and stored in Bash
    history:

    ```Bash
    cd DIRECTORY
    ```

    Where `DIRECTORY` is what user selected during expansion of the original
    command.

`CTRL+k`
:   Calls and expands:

    ```Bash
    TOOLSET cd --self-command
    ```

    Into following command which ends up being executed and stored in Bash
    history:

    ```Bash
    TOOLSET cd DIRECTORY
    ```

    Where `DIRECTORY` is what user selected during expansion of the original
    command.


# FILES

`${XDG_CONFIG_HOME:-$HOME/.config}/${toolset}/command-wrapper-cd.dhall`
:   Configuration file specifies list of directories that are listed when this
    subcommand is invoked, and it allows to configure selection tool and
    terminal emulator command.

    See also `XDG_CONFIG_HOME` in *ENVIRONMENT VARIABLES* section for more
    information on how Command Wrapper figures out where to look for this
    configuration file.


# ENVIRONMENT VARIABLES

See also `command-wrapper(1)` *ENVIRONMENT VARIABLES* section.  Everything
mentioned there applies to this subcommand as well.

`XDG_CONFIG_HOME`
:   Overrides where this subcommand expects its configuration file.  It follows
    this simple logic:

    * If `XDG_CONFIG_HOME` environment variable is set then the configuration
      file has path:

        ```
        ${XDG_CONFIG_HOME}/${toolset}/command-wrapper-cd.dhall
        ```

    * If `XDG_CONFIG_HOME` environment variable is not set then default value
      is used instead:

        ```
        ${HOME}/.config/${toolset}/command-wrapper-cd.dhall
        ```

    See [XDG Base Directory Specification
    ](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html)
    for more information on rationale behind this.

`CD_LEVEL=`*DEPTH*
:   This environment variable is passed to shell executed by `cd` subcommand.
    It informs how many nested `cd` calls were made.  Very useful when trying
    to figure out how many shells one needs to terminate.

`CD_DIRECTORY=`*DIR*
:   Another environment variable that is passed to shell executed by `cd`
    subcommand.  It contains directory path that was selected when `cd` was
    invoked.  It comes handy when one needs to jump back to that directory or
    figure out what was the purpose of the last directory jump.  To jump back
    into this directory one can use:

    ```
    cd "$CD_DIRECTORY"
    ```


# SEE ALSO

command-wrapper(1)

* [XDG Base Directory Specification
  ](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html)
* [FZF -- A command-line fuzzy finder](https://github.com/junegunn/fzf)
* [FZF -- Related Projects](https://github.com/junegunn/fzf/wiki/Related-projects)
  page includes links to similar tools that could be used instead of FZF.
* [fzy -- fast, simple fuzzy text selector for the terminal with an advanced
  scoring algorithm](https://github.com/jhawthorn/fzy)
* [Skim -- Fuzzy Finder in rust!](https://github.com/lotabout/skim)


# BUGS

<https://github.com/trskop/command-wrapper/issues>
