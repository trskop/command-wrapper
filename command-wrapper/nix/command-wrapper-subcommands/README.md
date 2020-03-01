# Directory Structure

```
/nix/store/${hash}-command-wrapper-toolset-${toolset}-subcommands/
├── libexec/
│   └── ${toolset}/
│       ├── ${toolst}-${subcommand0}
│       └── ...
└── share/
    └── man/
        └── man1/
            ├── ${toolset}-${subcommand0}.1.gz
            └── ...
```

With the above directory structure the toolset needs to be configured to lookup
for subcommands in the:

```
/nix/store/${hash}-command-wrapper-toolset-${toolset}-subcommands/libexec/${toolset}
```

And for manual pages in:

```
/nix/store/${hash}-command-wrapper-toolset-${toolset}-subcommands/share/man
```
