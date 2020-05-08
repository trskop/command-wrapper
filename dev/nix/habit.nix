{ pkgs ? import <nixpkgs> { } }:

rec {
  command-wrapper =
    pkgs.callPackage ../../command-wrapper/nix/command-wrapper { };

  toolset = pkgs.callPackage ../../command-wrapper/nix/command-wrapper-toolset {
    toolset = "habit";
    inherit command-wrapper;
  };

  buildInputs = [ toolset ];

  # See also `command-wrapper-direnv-library(7)`.  If these variables wouldn't
  # be provided here then they would be configured by `use_command_wrapper()`.
  # The advantage of defining them here is that there is less work for `.envrc`
  # to do.
  env = let
    dhallLibDir = "${command-wrapper}/etc/command-wrapper/lib";
    toolsetShare = "${toolset}/share";
  in {
    COMMAND_WRAPPER_LIB = "${dhallLibDir}/CommandWrapper/package.dhall";
    COMMAND_WRAPPER_EXEC_LIB = "${dhallLibDir}/Exec/package.dhall";
    COMMAND_WRAPPER_PRELUDE_LIB = "${dhallLibDir}/Prelude/package.dhall";

    HABIT_BASH_COMPLETION =
      "${toolsetShare}/bash-completion/completions/habit.bash";
    HABIT_FISH_COMPLETION =
      "${toolsetShare}/fish/vendor_completions.d/habit.fish";
    HABIT_ZSH_COMPLETION = "${toolsetShare}/zsh/vendor_completions/_habit";
  };
}
