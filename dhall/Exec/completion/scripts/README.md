# Completion Scripts

Shell scripts or templates of shell scripts for command line completion that
can be integrated into Command Wrapper by using following Command Wrapper
completion feature:

```
TOOLSET completion --wrapper --expression=EXPRESSION --exec [-- [ARGUMENT ...]]
```

For more details see:

```
TOOLSET help [--man] completion
```

Scripts ending with `.bash` extensions are standard Bash scripts, those ending
with `.bash.dhall` are templates of Bash scripts, i.e. Dhall expressions of
type `Text`.  All scripts have the following calling convention:

```
SCRIPT --index=INDEX [--shell=SHELL] [-- [WORD ...]]

SCRIPT {--help|-h}
```

Individual scripts may deviate from the calling convention slightly, but the
above should always work.  Most of them will even default `INDEX` to `0` if
that option is not specified.


## Description Of Individual Scripts


*   [`bazel.bash`](./bazel.bash) is a script that finds Bazel completion on
    your system and uses it.  See its `findCompletion` function for more
    details on how its done.

    ```
    bazel.bash [--index=NUM] [--shell=SHELL] [-- [WORD ...]]

    bazel.bash {--help|-h}
    ```

*   [`direnv.bash`](./direnv.bash) is a command line completion for `direnv`
    created from scratch with the Command Wrappery style options.

    ```
    direnv.bash [--hide-private-commands] [--index=NUM] [--shell=SHELL]
        [-- [WORD ...]]

    direnv.bash {--help|-h}
    ```

*   [`docker-compose.bash`](./docker-compose.bash) is a script that finds
    `docker-compose` completion on your system and uses it.  See its
    `findCompletion` function for more details on how its done.

    ```
    docker-compose.bash [--index=NUM] [--shell=SHELL] [-- [WORD ...]]

    docker-compose.bash {--help|-h}
    ```

*   [`go-jira.bash`](./go-jira.bash) is a script that provides Command Wrapper
    style API for command line completion embedded in `go-jira` tool.

    ```
    go-jira.bash [--index=NUM] [--shell=SHELL] [-- [WORD ...]]

    go-jira.bash {--help|-h}
    ```

*   [`jq.bash.dhall`](./jq.bash.dhall) is a Dhall template of a Bash script
    that provides command line completion for `jq`.  As part of the template it
    imports script from <https://github.com/perlpunk/shell-completions/>
    repository.

    ```
    TOOLSET completion --wrapper --expression='./jq.bash.dhall' --exec --
        [--index=NUM] [--shell=SHELL] [-- [WORD ...]]

    TOOLSET completion --wrapper --expression='./jq.bash.dhall' --exec --
        {--help|-h}
    ```


*   [`nix.bash.dhall`](./nix.bash.dhall) is a Dhall template of a Bash script
    that provides command line completion for Nix and NixOS commands.  As part
    of the template it imports script from
    <https://github.com/hedning/nix-bash-completions/> repository.

    ```
    TOOLSET completion --wrapper --expression='./nix.bash.dhall' --exec --
        [--index=NUM] [--shell=SHELL] [-- [WORD ...]]

    TOOLSET completion --wrapper --expression='./nix.bash.dhall' --exec --
        {--help|-h}
    ```

*   [`yarn.bash.dhall`](./yarn.bash.dhall) is a Dhall template of a Bash script
    that provides command line completion for Yarn tool.  As part of the
    template it imports script from
    <https://github.com/dsifford/yarn-completion/> repository.

    ```
    TOOLSET completion --wrapper --expression='./yarn.bash.dhall' --exec --
        [--index=NUM] [--shell=SHELL] [-- [WORD ...]]

    TOOLSET completion --wrapper --expression='./yarn.bash.dhall' --exec --
        {--help|-h}
    ```
