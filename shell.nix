{ pkgs ? import <nixpkgs> { } }:

let
  toolset = pkgs.callPackage ./command-wrapper/nix/command-wrapper-toolset {
    toolset = "something";
    subcommands = [
      (toolset:
        pkgs.callPackage ./command-wrapper/nix/command-wrapper-subcommands {
          inherit toolset;
        })
    ];
  };
in pkgs.mkShell {
  buildInputs = [ toolset ];

  LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
  TERMINFO_DIRS = "/etc/terminfo:/lib/terminfo:/usr/share/terminfo";
  shellHook = ''
    source "${toolset}/share/bash-completion/completions/command-wrapper.bash"
  '';
}
