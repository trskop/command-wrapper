-- vim: filetype=dhall
--
-- Dhall shell script templates for importing embedded libraries for
-- subcommands.

let -- At the moment only Bash is supported.
    Shell = < Bash >

let importBashLibrary =
      ''
      # Import Command Wrapper Bash library
      #
      # Environment variables `COMMAND_WRAPPER_NAME` and `COMMAND_WRAPPER_EXE` are
      # provided when by Command Wrapper when subcommand is executed.  For more
      # information see `command-wrapper-subcommand-protocol(7)` manual page.
      #
      # shellcheck source=/dev/null
      source <(
          COMMAND_WRAPPER_INVOKE_AS="''${COMMAND_WRAPPER_NAME}" "''${COMMAND_WRAPPER_EXE}" \
              completion --library --shell=bash --content
      )
      ''

in  λ(shell : Shell) → merge { Bash = importBashLibrary } shell
