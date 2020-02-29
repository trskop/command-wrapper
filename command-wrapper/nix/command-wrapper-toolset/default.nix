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
        version = "0.1.0.0-rc2";
      in "${repoUrl}/releases/download/${version}/command-wrapper-${version}.tar.xz";

      # This value can be taken from `${url}.sha256sum`, and converted using:
      #
      #   nix-hash --type sha256 --to-base32 "${sha256inBase16}"
      sha256 = "1fam4szk8i53db04qcvhsqp6gx76jfvca6fj8hl8ww2dcrc97l04";
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
    COMMAND_WRAPPER_INVOKE_AS="${toolset}" "$out/libexec/command-wrapper/command-wrapper" \
      completion --script --shell=bash \
      --output="$out/share/bash-completion/completions/command-wrapper.bash"

    mkdir -p "$out/share/fish/vendor_completions.d"
    COMMAND_WRAPPER_INVOKE_AS="${toolset}" "$out/libexec/command-wrapper/command-wrapper" \
      completion --script --shell=fish \
      --output="$out/share/fish/vendor_completions.d/command-wrapper.fish"

    mkdir -p "$out/share/zsh/vendor_completions"
    COMMAND_WRAPPER_INVOKE_AS="${toolset}" "$out/libexec/command-wrapper/command-wrapper" \
      completion --script --shell=zsh \
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
