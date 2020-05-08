# Usage example:
#
# ```Nix
# { pkgs ? import <nixpkgs> { } }:
#
# pkgs.callPackage ./nix/command-wrapper { }
# ```

{ stdenv, fetchurl, makeWrapper, lib }:

stdenv.mkDerivation rec {
  version = "0.1.0.0-rc9";
  name = "command-wrapper-${version}";

  src =
    # At the moment only Linux executables are provided.
    assert stdenv.isLinux;
    fetchurl {
      url = let repoUrl = "https://github.com/trskop/command-wrapper";
      in "${repoUrl}/releases/download/${version}/command-wrapper-${version}.tar.xz";

      # This value can be taken from `${url}.sha256sum`, and converted using:
      #
      #   wget -q -O- "${url}" | cut -f1 -d' ' | xargs nix-hash --type sha256 --to-base32
      sha256 = "0pmr3l0ck7057fc6qvpir5gmihqsbk2zfmmpj7cq8lwil4arhw97";
    };

  defaultConfig = builtins.readFile ./default.dhall;

  dontUnpack = true;
  dontConfigure = true;
  dontBuild = true;
  dontStrip = true;
  dontPatchELF = true;
  noAuditTmpdir = true;

  buildInputs = [ makeWrapper ];

  installPhase = ''
    mkdir -p "$out/etc/command-wrapper"
    tar -C "$out" -xf "$src" --strip-components=1
    chmod u+w "$out/share"

    declare -r commandWrapper="$out/libexec/command-wrapper/command-wrapper"

    declare -r dhallLibDir="$out/etc/command-wrapper/lib"
    declare -r -A dhallLibraries=(
      [command-wrapper]="''${dhallLibDir}/CommandWrapper"
      [exec]="''${dhallLibDir}/Exec"
      [prelude]="''${dhallLibDir}/Prelude"
    )

    for lib in "''${!dhallLibraries[@]}"; do
      libraryDir="''${dhallLibraries[''${lib}]}"
      libraryContentFile="''${libraryDir}/library.dhall"
      libraryPackageFile="''${libraryDir}/package.dhall"

      mkdir -p "''${libraryDir}"

      "''${commandWrapper}" completion --library --dhall="''${lib}" --content \
        > "''${libraryContentFile}"
      "''${commandWrapper}" config --dhall-freeze \
        --no-remote-only --for-security \
        --expression="''${libraryContentFile}" \
        --output="''${libraryPackageFile}"
    done

    mkdir -p "$out/etc/command-wrapper/default"
    "''${commandWrapper}" config --dhall \
      --let="CommandWrapper=''${dhallLibDir}/CommandWrapper/package.dhall" \
      --let="Prelude=''${dhallLibDir}/Prelude/package.dhall" \
      --let="out=\"$out\"" \
      --output="$out/etc/command-wrapper/default/constructor.dhall" <<'EOF'
    ${defaultConfig}
    EOF
    "''${commandWrapper}" config --dhall-freeze \
      --no-remote-only --for-security \
      --expression="$out/etc/command-wrapper/default/constructor.dhall" \
      --output="$out/etc/command-wrapper/default.dhall"
  '';
}
