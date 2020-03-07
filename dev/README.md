# Command Wrapper Toolset `cw-dev`


## Requirements:

*   [Direnv](https://direnv.net/)
*   [Nix](https://nixos.org/nix/)
*   [Stack](https://docs.haskellstack.org/)
*   Docker (Required only for building static executables)


## Usage:

```
cw-dev [GLOBAL_OPTIONS] {build|build-static|install|test}

cw-dev [GLOBAL_OPTIONS] SUBCOMMAND [--] [SUBCOMMAND_ARGUMENTS]

cw-dev [GLOBAL_OPTIONS] help [SUBCOMMAND]

cw-dev [GLOBAL_OPTIONS] config [SUBCOMMAND] [CONFIG_OPTIONS]

cw-dev [GLOBAL_OPTIONS] completion [COMPLETION_OPTIONS]

cw-dev [GLOBAL_OPTIONS] version [VERSION_OPTIONS]

cw-dev {--help|-h}

cw-dev {--version|-V}
```


## Directory Structure

```
${REPO}/dev/
  ├── config/
  │   ├── cw-dev/
  │   │   ├── command-wrapper-exec.dhall
  │   │   ├── default/
  │   │   │   ├── constructor.dhall
  │   │   │   └── exec-aliases.dhall
  │   │   ├── default.dhall -> ${cw_dev_cache_dir}/default.dhall-${hash}.dhall
  │   │   ├── exec/
  │   │   │   └── commands.dhall
  │   │   └── options.dhall -> ${cw_dev_cache_dir}/options.dhall-${hash}.dhall
  │   └── lib/
  │       ├── CommandWrapper -> ${cw_dev_cache_dir}/CommandWrapper-${hash}.dhall
  │       ├── Exec -> ${cw_dev_cache_dir}/Exec-${hash}.dhall
  │       └── Prelude -> ${cw_dev_cache_dir}/Prelude-${hash}.dhall
  ├── cw-dev.envrc
  ├── dot.envrc
  ├── nix
  │   └── cw-dev.nix
  └── shell.nix
```
