# Usage example:
#
# ```
# { pkgs ? import <nixpkgs> { } }:
# 
# pkgs.callPackage ./nix/command-wrapper-toolset { toolset = "yx"; }
# ```

{ stdenv, fetchurl, makeWrapper, toolset }:

stdenv.mkDerivation rec {
  name = "command-wrapper-toolset-${toolset}";

  src =
    # At the moment only Linux executables are provided.
    assert stdenv.isLinux;
    fetchurl {
      url = let
        repoUrl = "https://github.com/trskop/command-wrapper";
        version = "0.1.0.0-rc1";
      in "${repoUrl}/releases/download/${version}/command-wrapper-${version}.tar.xz";

      # This value can be taken from `${url}.sha256sum`, and converted using:
      #
      #   nix-hash --type sha256 --to-base32 "${sha256inBase16}"
      sha256 = "1kdmmlmmgvk8zf3jms80r73nb90jyhimqw3v1ba02pgyw71b9z5d";
    };

  dontUnpack = true;
  dontConfigure = true;
  dontBuild = true;
  dontStrip = true;
  dontPatchELF = true;
  noAuditTmpdir = true;

  buildInputs = [ makeWrapper ];

  installPhase = ''
    mkdir -p "$out/bin"
    tar -C "$out" -xf "$src"

    makeWrapper "$out/libexec/command-wrapper/command-wrapper" \
      "$out/bin/${toolset}" \
      --argv0 "${toolset}" \
      --set TERMINFO_DIRS "/etc/terminfo:/lib/terminfo:/usr/share/terminfo" \
      --prefix COMMAND_WRAPPER_PATH : "$out/libexec/command-wrapper" \
      --prefix COMMAND_WRAPPER_MANPATH : "$out/share/man"
  '';
}
