# Directory Structure

```
/nix/store/${hash}-command-wrapper/
├── etc/
│   └── command-wrapper/
│       ├── command-wrapper/
│       │   ├── default/
│       │   │   └── constructor.dhall
│       │   └── default.dhall -- Imports default/constructor.dhall
│       └── lib/
│           ├── CommandWrapper
│           │   ├── library.dhall  -- Content of the library
│           │   └── package.dhall  -- Hash protected import of library.dhall
│           ├── Exec
│           │   ├── library.dhall
│           │   └── package.dhall
│           └── Prelude
│               ├── library.dhall
│               └── package.dhall
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
    └── man/
        ├── man1/
        │   ├── command-wrapper.1.gz
        │   ├── command-wrapper-cd.1.gz
        │   ├── command-wrapper-completion.1.gz
        │   ├── command-wrapper-config.1.gz
        │   ├── command-wrapper-exec.1.gz
        │   ├── command-wrapper-help.1.gz
        │   ├── command-wrapper-skel.1.gz
        │   └── command-wrapper-version.1.gz
        └── man7/
            ├── command-wrapper-bash-library.7.gz
            └── command-wrapper-subcommand-protocol.7.gz
```

```
/nix/store/${hash}-command-wrapper-toolset-${toolset}/
├── bin/
│   └── ${toolset}
├── etc/
│   └── command-wrapper/
│       ├── command-wrapper/
│       │   └── default.dhall -- Imports /nix/store/${hash}-command-wrapper/default/constructor.dhall
│       └── ${toolset}/
│           ├── default/
│           │   └── constructor.dhall
│           └── default.dhall -- Imports default/constructor.dhall
├── libexec/
│   ├── command-wrapper/
│   │   └── command-wrapper  -- Nix-style wrapper scripts for /nix/store/${hash}-command-wrapper/libexec/command-wrapper/command-wrapper
│   └── ${toolset}/
│       └── ${toolset}-*
└── share/
    ├── doc/
    │   └── ${toolset}/
    │       ├── ${toolset}-*.1.html
    │       └── ${toolset}.1.html
    ├── man/
    │   └── man1/
    │       ├── ${toolset}-*.1.gz
    │       └── ${toolset}.1.gz
    ├── bash-completion/
    │   └── completions/
    │       └── ${toolset}.bash
    ├── fish/
    │   └── vendor_completions.d/
    │       └── ${toolset}.fish
    └── zsh/
        └── vendor_completions/
            └── _${toolset}
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
