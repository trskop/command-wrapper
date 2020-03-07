{ pkgs ? import <nixpkgs> { } }:

rec {
  toolset = pkgs.callPackage ../../command-wrapper/nix/command-wrapper-toolset {
    toolset = "cw-dev";
  };

  buildInputs = [ toolset ];

  env = {
    COMMAND_WRAPPER_LIB = "${toolset}/etc/command-wrapper/lib/CommandWrapper";
    COMMAND_WRAPPER_EXEC_LIB = "${toolset}/etc/command-wrapper/lib/Exec";
    COMMAND_WRAPPER_PRELUDE_LIB = "${toolset}/etc/command-wrapper/lib/Prelude";

    CW_DEV_BASH_COMPLETION =
      "${toolset}/share/bash-completion/completions/command-wrapper.bash";
    CW_DEV_FISH_COMPLETION =
      "${toolset}/share/fish/vendor_completions.d/command-wrapper.fish";
    CW_DEV_ZSH_COMPLETION =
      "${toolset}/share/zsh/vendor_completions/_command-wrapper";
  };
}
