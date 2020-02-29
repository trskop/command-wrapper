# Usage example:
#
# ```
# { pkgs ? import <nixpkgs> { } }:
# 
# pkgs.callPackage ./nix/command-wrapper-toolset { toolset = "yx"; }
# ```

{ stdenv, fetchurl, makeWrapper, lib, toolset, subcommands ? [ ] }:

let
  toolsetVersion = "1.0.0";

  toolsetSubcommands = map (pkg:
    pkg {
      name = toolset;
      manpage = toolset;
      version = toolsetVersion;
    }) subcommands;

in stdenv.mkDerivation rec {
  name = "command-wrapper-toolset-${toolset}";
  version = toolsetVersion;

  src =
    # At the moment only Linux executables are provided.
    assert stdenv.isLinux;
    fetchurl {
      url = let
        repoUrl = "https://github.com/trskop/command-wrapper";
        version = "0.1.0.0-rc3";
      in "${repoUrl}/releases/download/${version}/command-wrapper-${version}.tar.xz";

      # This value can be taken from `${url}.sha256sum`, and converted using:
      #
      #   nix-hash --type sha256 --to-base32 "${sha256inBase16}"
      sha256 = "03lf2jysjd45ji0vx7vnxl1b4d0dqs57r07s3lk7hx43c4g2xw8v";
    };

  dontUnpack = true;
  dontConfigure = true;
  dontBuild = true;
  dontStrip = true;
  dontPatchELF = true;
  noAuditTmpdir = true;

  buildInputs = [ makeWrapper ] ++ toolsetSubcommands;

  installPhase = ''
    mkdir -p "$out/bin" "$out/etc"
    tar -C "$out" -xf "$src" --strip-components=1
    chmod u+w "$out/share"

    makeWrapper "$out/libexec/command-wrapper/command-wrapper" \
      "$out/bin/${toolset}" \
      --argv0 "${toolset}" \
      --set TERMINFO_DIRS "/etc/terminfo:/lib/terminfo:/usr/share/terminfo" \
      --set COMMAND_WRAPPER_SYSTEM_CONFIG_DIR "$out/libexec/command-wrapper/etc" \
      --prefix COMMAND_WRAPPER_PATH : "$out/libexec/command-wrapper" \
      --prefix COMMAND_WRAPPER_PATH : "${
        lib.makeSearchPath "libexec/${toolset}" toolsetSubcommands
      }" \
      --prefix COMMAND_WRAPPER_MANPATH : "$out/share/man" \
      --prefix COMMAND_WRAPPER_MANPATH : "${
        lib.makeSearchPath "share/man" toolsetSubcommands
      }"

    mkdir -p "$out/share/bash-completion/completions"
    "$out/libexec/command-wrapper/command-wrapper" \
      completion --script --shell=bash \
      --toolset="${toolset}" \
      --executable="$out/bin/${toolset}" \
      --output="$out/share/bash-completion/completions/command-wrapper.bash"

    mkdir -p "$out/share/fish/vendor_completions.d"
    "$out/libexec/command-wrapper/command-wrapper" \
      completion --script --shell=fish \
      --toolset="${toolset}" \
      --executable="$out/bin/${toolset}" \
      --output="$out/share/fish/vendor_completions.d/command-wrapper.fish"

    mkdir -p "$out/share/zsh/vendor_completions"
    "$out/libexec/command-wrapper/command-wrapper" \
      completion --script --shell=zsh \
      --toolset="${toolset}" \
      --executable="$out/bin/${toolset}" \
      --output="$out/share/zsh/vendor_completions/_command-wrapper"

    mkdir -p "$out/etc/command-wrapper/lib"
    "$out/libexec/command-wrapper/command-wrapper" \
      completion --library --dhall=command-wrapper --import \
      > "$out/etc/command-wrapper/lib/CommandWrapper"
    "$out/libexec/command-wrapper/command-wrapper" \
      completion --library --dhall=exec --import \
      > "$out/etc/command-wrapper/lib/Exec"
    "$out/libexec/command-wrapper/command-wrapper" \
      completion --library --dhall=prelude --import \
      > "$out/etc/command-wrapper/lib/Prelude"
  '';
}
