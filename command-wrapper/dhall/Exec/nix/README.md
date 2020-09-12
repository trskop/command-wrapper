# Nix

[Nix](https://nixos.org/nix/manual/) is cross-platform package manager used by
NixOS that is available on other platforms as well. It can be used as an
alternative to package managers like Homebrew.

Homepage: [nixos.org](https://nixos.org)

Documentation: [Nix Package Manager Guide](https://nixos.org/nix/manual/)


## Usage Example

```Dhall
let CommandWrapper =
      https://raw.githubusercontent.com/trskop/command-wrapper/master/command-wrapper/dhall/CommandWrapper/package.dhall

let Exec =
      https://raw.githubusercontent.com/trskop/command-wrapper/master/command-wrapper/dhall/Exec/package.dhall

let Prelude =
      https://prelude.dhall-lang.org/v18.0.0/package.dhall sha256:10db3c919c25e9046833df897a8ffe2701dc390fa0893d958c3430524be5a43e

let defaultToolset = "yx"

let toolset = env:COMMAND_WRAPPER_EXE as Text ? defaultToolset

let -- List of nix commands that we want to provide. This is just a subset of
    -- what's in Exec.nix.Command.Type
    nixCommands =
      [ Exec.nix.Command.Type.nix
      , Exec.nix.Command.Type.nix-build
      , Exec.nix.Command.Type.nix-channel
      , Exec.nix.Command.Type.nix-collect-garbage
      , Exec.nix.Command.Type.nix-copy-closure
      , Exec.nix.Command.Type.nix-env
      , Exec.nix.Command.Type.nix-hash
      , Exec.nix.Command.Type.nix-install-package
      , Exec.nix.Command.Type.nix-instantiate
      , Exec.nix.Command.Type.nix-prefetch-url
      , Exec.nix.Command.Type.nix-push
      , Exec.nix.Command.Type.nix-shell
      , Exec.nix.Command.Type.nix-store
      ]

let nixCommandToExecNamedCommand =
      λ(nix-command : Exec.nix.Command.Type) →
        let name = Exec.nix.Command.show nix-command

        let noArguments = [] : List Text

        in  CommandWrapper.ExecNamedCommand::{
            , name
            , description = Some
                "Just invoke '${name}', but with command-line completion."
            , command =
                Exec.nix.command Exec.nix.Options::{=} nix-command noArguments
            , completion = Some
                ( Exec.nix.completion
                    toolset
                    nix-command
                    (None Text)
                    noArguments
                )
            }

in  Prelude.List.map
      Exec.nix.Command.Type
      CommandWrapper.ExecNamedCommand.Type
      nixCommandToExecNamedCommand
      nixCommands
```
