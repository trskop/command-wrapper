# Directory Structure

```
/nix/store/${hash}-command-wrapper-toolset-${toolset}/
├── bin/  -- Nix-style wrapper scripts for command-wrapper binary in libexec
│   ├── command-wrapper
│   └── ${toolset}
├── etc/
│   └── command-wrapper/
│       ├── command-wrapper/
│       │   ├── default/
│       │   │   └── constructor.dhall
│       │   └── default.dhall -- Imports default/constructor.dhall
│       └── lib/
│           ├── CommandWrapper
│           ├── Exec
│           └── Prelude
├── libexec/
│   └── command-wrapper/
│       ├── command-wrapper
│       ├── command-wrapper-cd
│       ├── command-wrapper-exec
│       └── command-wrapper-skel
└── share/
    ├── doc/
    │   └── command-wrapper/
    │       ├── command-wrapper.1.html
    │       ├── command-wrapper-bash-library.7.html
    │       ├── command-wrapper-cd.1.html
    │       ├── command-wrapper-completion.1.html
    │       ├── command-wrapper-config.1.html
    │       ├── command-wrapper-exec.1.html
    │       ├── command-wrapper-help.1.html
    │       ├── command-wrapper-skel.1.html
    │       ├── command-wrapper-subcommand-protocol.7.html
    │       └── command-wrapper-version.1.html
    ├── man/
    │   ├── man1/
    │   │   ├── command-wrapper.1.gz
    │   │   ├── command-wrapper-cd.1.gz
    │   │   ├── command-wrapper-completion.1.gz
    │   │   ├── command-wrapper-config.1.gz
    │   │   ├── command-wrapper-exec.1.gz
    │   │   ├── command-wrapper-help.1.gz
    │   │   ├── command-wrapper-skel.1.gz
    │   │   └── command-wrapper-version.1.gz
    │   └── man7/
    │       ├── command-wrapper-bash-library.7.gz
    │       └── command-wrapper-subcommand-protocol.7.gz
    ├── bash-completion/
    │   └── completions/
    │       └── command-wrapper.bash
    ├── fish/
    │   └── vendor_completions.d/
    │       └── command-wrapper.fish
    └── zsh/
        └── vendor_completions/
            └── _command-wrapper
```

With the above directory structure Command Wrapper needs to be configured to
lookup for subcommands in the:

```
/nix/store/${hash}-command-wrapper-toolset-${toolset}/libexec/command-wrapper
```

For manual pages in:

```
/nix/store/${hash}-command-wrapper-toolset-${toolset}/share/man
```

And for its configuration in:

```
/nix/store/${hash}-command-wrapper-toolset-${toolset}/etc
```
