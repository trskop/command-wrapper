{ pkgs ? import <nixpkgs> { } }:

rec {
  toolset = pkgs.callPackage ../../command-wrapper/nix/command-wrapper-toolset {
    toolset = "habit";
  };

  buildInputs = [ toolset ];

  # See also `command-wrapper-direnv-library(7)`.  If these variables wouldn't
  # be provided here then they would be configured by `use_command_wrapper()`.
  # The advantage of defining them here is that there is less work for `.envrc`
  # to do.
  env = {
    COMMAND_WRAPPER_LIB = "${toolset}/etc/command-wrapper/lib/CommandWrapper";
    COMMAND_WRAPPER_EXEC_LIB = "${toolset}/etc/command-wrapper/lib/Exec";
    COMMAND_WRAPPER_PRELUDE_LIB = "${toolset}/etc/command-wrapper/lib/Prelude";

    HABIT_BASH_COMPLETION =
      "${toolset}/share/bash-completion/completions/command-wrapper.bash";
    HABIT_FISH_COMPLETION =
      "${toolset}/share/fish/vendor_completions.d/command-wrapper.fish";
    HABIT_ZSH_COMPLETION =
      "${toolset}/share/zsh/vendor_completions/_command-wrapper";
  };
}
