-- vim: filetype=dhall

let Command =
        ./Type.dhall sha256:dc2b111daadb4265396dc562b757f208189242a884e704a4298686ce8984af27
      ? ./Type.dhall

let show =
      λ(command : Command) →
        merge
          { nix = "nix"
          , nix-build = "nix-build"
          , nix-channel = "nix-channel"
          , nix-collect-garbage = "nix-collect-garbage"
          , nix-copy-closure = "nix-copy-closure"
          , nix-env = "nix-env"
          , nix-hash = "nix-hash"
          , nix-install-package = "nix-install-package"
          , nix-instantiate = "nix-instantiate"
          , nix-prefetch-url = "nix-prefetch-url"
          , nix-push = "nix-push"
          , nix-shell = "nix-shell"
          , nix-store = "nix-store"
          , nixos-build-vms = "nixos-build-vms"
          , nixos-container = "nixos-container"
          , nixos-generate-config = "nixos-generate-config"
          , nixos-install = "nixos-install"
          , nixos-option = "nixos-option"
          , nixos-rebuild = "nixos-rebuild"
          , nixos-version = "nixos-version"
          }
          command

in  show : Command → Text
