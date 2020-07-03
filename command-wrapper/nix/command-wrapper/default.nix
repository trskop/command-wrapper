# Usage example:
#
# ```Nix
# { pkgs ? import <nixpkgs> { } }:
#
# pkgs.callPackage ./nix/command-wrapper { }
# ```

{ stdenv, fetchurl, makeWrapper, lib }:

stdenv.mkDerivation rec {
  version = "0.1.0.0-rc10";
  name = "command-wrapper-${version}";

  src =
    # At the moment only Linux executables are provided.
    assert stdenv.isLinux;
    fetchurl {
      url = let repoUrl = "https://github.com/trskop/command-wrapper";
      in "${repoUrl}/releases/download/${version}/command-wrapper-${version}.tar.xz";

      # This value can be taken from `${url}.sha256nix`:
      sha256 = "1pymcp4xaxp59rg28kaz4p0n30flczwn6461wvnpl8czmrkvp7yp";
    };

  defaultConfig = builtins.readFile ./default.dhall;
  mkEnvironmentVariablesDhall = builtins.readFile ./environment-variables.dhall;

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

    # These values have to match what's in environment-variables.dhall
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

    "''${commandWrapper}" config --dhall \
      --let="CommandWrapper=''${dhallLibDir}/CommandWrapper/package.dhall" \
      --let="Prelude=''${dhallLibDir}/Prelude/package.dhall" \
      --let="dhallLibDir=\"''${dhallLibDir}\"" \
      --output="$out/etc/command-wrapper/environment-variables.dhall" <<'EOF'
    ${mkEnvironmentVariablesDhall}
    EOF
  '';
}
