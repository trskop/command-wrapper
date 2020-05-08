# Usage example:
#
# ```Nix
# { pkgs ? import <nixpkgs> { } }:
#
# rec {
#   command-wrapper = pkgs.callPackage ./nix/command-wrapper { };
#
#   toolset = pkgs.callPackage ./nix/command-wrapper-toolset {
#     toolset = "yx";
#     inherit command-wrapper;
#   };
# }
# ```

{ stdenv, fetchurl, makeWrapper, lib, command-wrapper, toolset
, toolsetVersion ? "1.0.0", subcommands ? [ ] }:

let
  version = toolsetVersion;

  toolsetSubcommands = map (pkg:
    pkg {
      name = toolset;
      manpage = toolset;
      inherit version;
    }) subcommands;

  toList = subDir: paths:
    if paths == [ ] then
      "[ ] : List Text"
    else
      ''[ "'' + (lib.concatStringsSep ''", "''
        (builtins.map (path: path + "/" + subDir)
          (builtins.filter (x: x != null) paths))) + ''" ] : List Text'';

  defaultConfig = paths: ''
    global // {
    , searchPath = global.searchPath # (${toList "libexec/${toolset}" paths})
    , manPath = [ "''${out}/share/man" ] # (${toList "share/man" paths})
    }
  '';

in stdenv.mkDerivation rec {
  name = "command-wrapper-toolset-${toolset}-${version}";
  inherit version;

  src = null;

  dontUnpack = true;
  dontConfigure = true;
  dontBuild = true;
  dontStrip = true;
  dontPatchELF = true;
  noAuditTmpdir = true;

  buildInputs = [ command-wrapper makeWrapper ] ++ toolsetSubcommands;

  installPhase = ''
    declare -r -a directories=(
      "$out/bin"
      "$out/etc/command-wrapper"
      # "$out/etc/habit"
      # "$out/share/man/man1"
    )
    mkdir -p "''${directories[@]}"

    declare -r commandWrapper="${command-wrapper}/libexec/command-wrapper/command-wrapper"
    declare -r commandWrapperConfigDir="${command-wrapper}/etc/command-wrapper"

    makeWrapper "''${commandWrapper}" \
      "$out/bin/command-wrapper" \
      --set TERMINFO_DIRS "/etc/terminfo:/lib/terminfo:/usr/share/terminfo" \
      --set COMMAND_WRAPPER_SYSTEM_CONFIG_DIR "$out/etc" \
      --set COMMAND_WRAPPER_FACADE "$out/bin/command-wrapper"

    makeWrapper "''${commandWrapper}" \
      "$out/bin/${toolset}" \
      --argv0 "${toolset}" \
      --set TERMINFO_DIRS "/etc/terminfo:/lib/terminfo:/usr/share/terminfo" \
      --set COMMAND_WRAPPER_SYSTEM_CONFIG_DIR "$out/etc" \
      --set COMMAND_WRAPPER_FACADE "$out/bin/command-wrapper"

    declare -r -A completions=(
      [bash]="$out/share/bash-completion/completions/${toolset}.bash"
      [fish]="$out/share/fish/vendor_completions.d/${toolset}.fish"
      [zsh]="$out/share/zsh/vendor_completions/_${toolset}"
    )

    for shell in "''${!completions[@]}"; do
      completionFile="''${completions[''${shell}]}"
      completionDir="$(dirname "''${completionFile}")"
      mkdir -p "''${completionDir}"
      "''${commandWrapper}" \
        completion --script --shell="''${shell}" \
        --toolset="${toolset}" \
        --executable="$out/bin/command-wrapper" \
        --output="''${completionFile}"
    done

    mkdir -p "$out/etc/command-wrapper/default"
    "''${commandWrapper}" config --dhall \
      --let="CommandWrapper=''${commandWrapperConfigDir}/lib/CommandWrapper/package.dhall" \
      --let="Prelude=''${commandWrapperConfigDir}/lib/Prelude/package.dhall" \
      --let="global=''${commandWrapperConfigDir}/default.dhall" \
      --let="out=\"$out\"" \
      --output="$out/etc/command-wrapper/default/constructor.dhall" <<'EOF'
    ${defaultConfig toolsetSubcommands}
    EOF
    "''${commandWrapper}" config --dhall-freeze \
      --no-remote-only --for-security \
      --expression="$out/etc/command-wrapper/default/constructor.dhall" \
      --output="$out/etc/command-wrapper/default.dhall"
  '';
}
