% COMMAND-WRAPPER-CD(1) Command Wrapper 0.1.0 | Command Wrapper
% Peter Trsko
% 23nd December 2018


# NAME

`command-wrapper-cd` -- Start a new subshell / Tmux window / terminal emulator
in a selected directory.


# USAGE

TOOLSET\_COMMAND \[GLOBAL\_OPTIONS] cd \[-s|\--shell|-t|\--tmux|-e|\--terminal]


# DESCRIPTION

Like `cd` shell command, but allows the user to select a directory from a
pre-defined list.  This speeds up access to commonly accessed directories.
Normally it spawns a new shell, or a Tmux window, see *OPTIONS* for details.

When no option is specified, `cd` subcommand checks if it's running in Tmux,
and if it is then it opens a new Tmux window.  If it's not executed inside Tmux
session then it executes a new instance of `$SHELL` in a selected directory.

Selecting directory from a list is implemented using external command.  Default
configuration uses [`fzf` command](https://github.com/junegunn/fzf), however,
there are other alternatives.


# OPTIONS

-s, \--shell
:   Execute a subshell even if in a Tmux session.

-t, \--tmux
:   Create a new Tmux window, or fail if not in Tmux.

-e, \--terminal
:   Open a new terminal emulator window.


# EXIT STATUS

TODO


# BASH KEY BINDINGS

Example of binding `TOOLSET_COMMAND cd` to CTRL-f:

```
bind '"\C-f":"TOOLSET_COMMAND cd\n"'
```

Please change `TOOLSET_COMMAND` to whatever you are using, or use full path to
`command-wrapper` executable.  Usually:

```
${HOME}/.local/lib/command-wrapper/command-wrapper
```


# FILES

`${XDG_CONFIG_HOME:-$HOME/.config}/command-wrapper/command-wrapper-cd.dhall`
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
        ${XDG_CONFIG_HOME}/command-wrapper/command-wrapper-cd.dhall
        ```

    * If `XDG_CONFIG_HOME` environment variable is not set then default value
      is used instead:

        ```
        ${HOME}/.config/command-wrapper/command-wrapper-cd.dhall
        ```

    See [XDG Base Directory Specification
    ](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html)
    for more information on rationale behind this.


# SEE ALSO

command-wrapper(1)

* [XDG Base Directory Specification
  ](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html)
* [FZF -- A command-line fuzzy finder](https://github.com/junegunn/fzf)
* [FZF -- Related Projects](https://github.com/junegunn/fzf/wiki/Related-projects)
  page includes links to similar tools that could be used instead of FZF.


# BUGS

<https://github.com/trskop/command-wrapper/issues>
