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

  toList = subDir: paths:
    if paths == [ ] then
      "[ ] : List Text"
    else
      ''[ "'' + (lib.concatStringsSep ''", "''
        (builtins.map (path: path + "/" + subDir)
          (builtins.filter (x: x != null) paths))) + ''" ] : List Text'';

  defaultConfig = paths: ''
    CommandWrapper.ToolsetConfig::{
    , aliases =
          [ CommandWrapper.SubcommandAlias::{
            , alias = "h"
            , description = Some "Shorthand for \"help\"."
            , command = "help"
            }
          , CommandWrapper.SubcommandAlias::{
            , alias = "man"
            , description = Some "Shorthand for \"help --man\"."
            , command = "help"
            , arguments = [ "--man" ]
            }
          ]
        # Prelude.List.map
            Text
            CommandWrapper.SubcommandAlias.Type
            (   λ(_ : Text)
              → CommandWrapper.SubcommandAlias::{
                , alias = "dhall''${_}"
                , description = Some "Shorthand for \"config --dhall''${_}\"."
                , command = "config"
                , arguments = [ "--dhall''${_}" ]
                }
            )
            [ ""
            , "-bash"
            , "-diff"
            , "-exec"
            , "-filter"
            , "-format"
            , "-freeze"
            , "-hash"
            , "-lint"
            , "-repl"
            , "-resolve"
            , "-text"
            ]
    , searchPath =
          [ "''${out}/libexec/command-wrapper" ]
        # (${toList "libexec/${toolset}" paths})
    , manPath = [ "''${out}/share/man" ] # (${toList "share/man" paths})
    }
  '';

in stdenv.mkDerivation rec {
  name = "command-wrapper-toolset-${toolset}";
  version = toolsetVersion;

  src =
    # At the moment only Linux executables are provided.
    assert stdenv.isLinux;
    fetchurl {
      url = let
        repoUrl = "https://github.com/trskop/command-wrapper";
        version = "0.1.0.0-rc5";
      in "${repoUrl}/releases/download/${version}/command-wrapper-${version}.tar.xz";

      # This value can be taken from `${url}.sha256sum`, and converted using:
      #
      #   nix-hash --type sha256 --to-base32 "${sha256inBase16}"
      sha256 = "10p9y12hbmf92cbbcphzig3s887zkhgv6id3jlx5l515qlncp8lh";
    };

  dontUnpack = true;
  dontConfigure = true;
  dontBuild = true;
  dontStrip = true;
  dontPatchELF = true;
  noAuditTmpdir = true;

  buildInputs = [ makeWrapper ] ++ toolsetSubcommands;

  installPhase = ''
    mkdir -p "$out/bin" "$out/etc/command-wrapper"
    tar -C "$out" -xf "$src" --strip-components=1
    chmod u+w "$out/share"

    makeWrapper "$out/libexec/command-wrapper/command-wrapper" \
      "$out/bin/command-wrapper" \
      --set TERMINFO_DIRS "/etc/terminfo:/lib/terminfo:/usr/share/terminfo" \
      --set COMMAND_WRAPPER_SYSTEM_CONFIG_DIR "$out/etc" \
      --set COMMAND_WRAPPER_FACADE "$out/bin/command-wrapper"

    makeWrapper "$out/libexec/command-wrapper/command-wrapper" \
      "$out/bin/${toolset}" \
      --argv0 "${toolset}" \
      --set TERMINFO_DIRS "/etc/terminfo:/lib/terminfo:/usr/share/terminfo" \
      --set COMMAND_WRAPPER_SYSTEM_CONFIG_DIR "$out/etc" \
      --set COMMAND_WRAPPER_FACADE "$out/bin/command-wrapper"

    declare -r -A completions=(
      [bash]="$out/share/bash-completion/completions/command-wrapper.bash"
      [fish]="$out/share/fish/vendor_completions.d/command-wrapper.fish"
      [zsh]="$out/share/zsh/vendor_completions/_command-wrapper"
    )

    for shell in "''${!completions[@]}"; do
      completionFile="''${completions[''${shell}]}"
      completionDir="$(dirname "''${completionFile}")"
      mkdir -p "''${completionDir}"
      "$out/libexec/command-wrapper/command-wrapper" \
        completion --script --shell="''${shell}" \
        --toolset="${toolset}" \
        --executable="$out/bin/command-wrapper" \
        --output="''${completionFile}"
    done

    declare -r -A dhallLibraries=(
      [command-wrapper]="$out/etc/command-wrapper/lib/CommandWrapper"
      [exec]="$out/etc/command-wrapper/lib/Exec"
      [prelude]="$out/etc/command-wrapper/lib/Prelude"
    )

    for lib in "''${!dhallLibraries[@]}"; do
      libraryFile="''${dhallLibraries[''${lib}]}"
      libraryDir="$(dirname "''${libraryFile}")"
      mkdir -p "''${libraryDir}"
      "$out/libexec/command-wrapper/command-wrapper" \
        completion --library --dhall="''${lib}" --import \
        > "''${libraryFile}"
    done

    "$out/libexec/command-wrapper/command-wrapper" \
        completion --library --dhall=command-wrapper --content \
        --output=./CommandWrapper
    "$out/libexec/command-wrapper/command-wrapper" \
        completion --library --dhall=prelude --content \
        --output=./Prelude
    mkdir -p "$out/etc/command-wrapper/default"
    "$out/libexec/command-wrapper/command-wrapper" config --dhall \
      --let="CommandWrapper=./CommandWrapper" --let="Prelude=./Prelude" \
      --let="out=\"$out\"" \
      --output="$out/etc/command-wrapper/default/constructor.dhall" <<'EOF'
    ${defaultConfig toolsetSubcommands}
    EOF
    "$out/libexec/command-wrapper/command-wrapper" config --dhall-freeze \
      --no-remote-only --for-security \
      --expression="$out/etc/command-wrapper/default/constructor.dhall" \
      --output="$out/etc/command-wrapper/default.dhall"
  '';
}
