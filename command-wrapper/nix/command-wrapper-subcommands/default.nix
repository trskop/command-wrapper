# Usage example:
#
# ```
# { pkgs ? import <nixpkgs> { } }:
#
# toolset = pkgs.callPackage ./nix/command-wrapper-toolset {
#   toolset = "yx";
#   subcommands = [
#     (toolset:
#       pkgs.callPackage ./nix/command-wrapper-subcommand { inherit toolset; })
#   ];
# };
# ```

{ stdenv, callPackage, fetchurl, fetchFromGitHub, makeWrapper, lib, toolset }:

let
  easy-dhall-nix = callPackage (fetchFromGitHub {
    owner = "justinwoo";
    repo = "easy-dhall-nix";
    rev = "cead668b72ef61d13c770b11c5bc20731aee82df"; # Dhall 1.30.0
    sha256 = "1120m0yc06l8mv5wd1bqmkbm65krsmqlnnm0di79nw19n3f0m0ci";
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
        ./man/toolset-$subcommand.1.md.dhall
          ${toolsetInfo}
          { upper = "''${subcommand^^*}"
          , command = "$subcommand"
          , exe = "${toolset.name}-$subcommand"
          }
          "${author}"
          "${date}"
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
