{ pkgs ? import <nixpkgs> { } }:

let habit = import ./nix/habit.nix { inherit pkgs; };

in pkgs.mkShell (habit.env // {
  buildInputs = habit.buildInputs
    ++ [ pkgs.fish pkgs.zsh pkgs.zsh-completions ];

  LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
  TERMINFO_DIRS = "/etc/terminfo:/lib/terminfo:/usr/share/terminfo";
})
