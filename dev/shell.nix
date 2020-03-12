{ pkgs ? import <nixpkgs> { } }:

let cw-dev = import ./nix/cw-dev.nix { inherit pkgs; };

in pkgs.mkShell (cw-dev.env // {
  buildInputs = cw-dev.buildInputs
    ++ [ pkgs.fish pkgs.zsh pkgs.zsh-completions ];

  LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
  TERMINFO_DIRS = "/etc/terminfo:/lib/terminfo:/usr/share/terminfo";
})
