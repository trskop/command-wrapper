# Usage example:
#
# ```Nix
# { pkgs ? import <nixpkgs> { } }:
#
# rec {
#   command-wrapper = pkgs.callPackage ./nix/command-wrapper { };
#
#   subcommands = [
#     (toolset:
#       pkgs.callPackage ./nix/command-wrapper-subcommand { inherit toolset; })
#   ];
#
#   toolset = pkgs.callPackage ./nix/command-wrapper-toolset {
#     toolset = "yx";
#     inherit command-wrapper subcommands;
#   };
# }
# ```

{ stdenv, callPackage, fetchurl, fetchFromGitHub, makeWrapper, lib, toolset }:

let
  easy-dhall-nix = callPackage (fetchFromGitHub {
    owner = "justinwoo";
    repo = "easy-dhall-nix";
    rev = "35bca5ba56b7b3f8684aa0afbb65608159beb5ce"; # Dhall 1.31.1
    sha256 = "16l71qzzfkv4sbxl03r291nswsrkr3g13viqkma2s8r5vy9la3al";
  }) { };

  pandoc = stdenv.mkDerivation rec {
    name = "pandoc";

    src = fetchurl {
      url = let
        repoUrl = "https://github.com/jgm/pandoc";
        version = "2.9.2";
        suffix = if stdenv.isLinux then "linux-amd64.tar.gz" else "macOS.zip";
      in "${repoUrl}/releases/download/${version}/pandoc-${version}-${suffix}";

      sha256 = if stdenv.isLinux then
        "022x364571xl2cy7pzybjmvyp6ds5nphdjwv8xlf5hb6c5dib7q3"
      else
        "17gr50kyzx663qy4y38zsjxcfi8jz81h76x5awglz8w0snx7c0al";
    };

    dontConfigure = true;
    dontBuild = true;
    dontStrip = true;
    dontPatchELF = true;
    noAuditTmpdir = true;

    installPhase = ''
      mkdir -p "$out/bin"
      install --mode=555 --target-directory="$out/bin" \
        bin/pandoc \
        bin/pandoc-citeproc
    '';
  };

  toolsetInfo = ''
    { name = "The ${toolset.name}"
    , manpage = "${toolset.manpage}"
    , upper = "${lib.toUpper toolset.name}"
    , version = "${toolset.version}"
    , command = "${toolset.name}"
    }
  '';

  author = "Peter Trsko";
  date = "24th February, 2020";

in stdenv.mkDerivation rec {
  name = "command-wrapper-toolset-${toolset.name}-subcommands";
  version = toolset.version;

  src = ./.;

  buildInputs =
    [ easy-dhall-nix.dhall-simple easy-dhall-nix.dhall-bash-simple pandoc ];

  dontConfigure = true;
  dontBuild = true;
  dontStrip = true;
  dontPatchELF = true;
  noAuditTmpdir = true;

  installPhase = ''
    mkdir -p \
      "$out/libexec/${toolset.name}" \
      "$out/share/man/man1"

    declare -a subcommands
    mapfile -t -d ''' subcommands < <(find ./bash -type f -print0)
    for subcommandInput in "''${subcommands[@]}"; do
      subcommand="$(basename "$subcommandInput")"
      subcommand="''${subcommand#toolset-}"
      if [[ "''${subcommand}" == *.dhall ]]; then
        subcommand="''${subcommand%.dhall}"
        dhall text --file="''${subcommandInput}" \
          > "''${subcommandInput%.dhall}"
      fi

      if [[ -e "''${subcommandInput%.dhall}" ]]; then
        install --mode=555 --no-target-directory \
          "''${subcommandInput%.dhall}" \
          "$out/libexec/${toolset.name}/${toolset.name}-$subcommand"
      fi

      if [[ -e "./man/toolset-$subcommand.1.md.dhall" ]]; then
        dhall text > "./man/toolset-$subcommand.1.md" <<EOF
        let T = ./man/toolset-$subcommand.1.md.dhall
        in  T.template
              T.Options::{
              , toolset = ${toolsetInfo}
              , subcommand =
                  { upper = "''${subcommand^^*}"
                  , command = "$subcommand"
                  , exe = "${toolset.name}-$subcommand"
                  }
              name = "${author}"
              date = "${date}"
              }
    EOF
      fi

      if [[ -f "./man/toolset-$subcommand.1.md" ]]; then
        pandoc --standalone --to=man \
          --output="$out/share/man/man1/${toolset.name}-$subcommand.1" \
          "./man/toolset-$subcommand.1.md"
      fi
    done
  '';
}
