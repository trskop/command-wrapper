-- vim: filetype=dhall
--
-- Dhall shell script templates for importing embedded Direnv library.

λ(toolset : Text) →
  ''
  # Import Command Wrapper Direnv library in `.envrc`.
  #
  # For more information see `command-wrapper-direnv-library(7)` manual page,
  # or call:
  #
  # ```Bash
  # ${toolset} help --man direnv-library
  # ```
  #
  # shellcheck source=/dev/null
  source <(${toolset} completion --library --direnv --content)
  
  # Example of how this library is used:
  #
  # use command-wrapper '${toolset}' './.command-wrapper'
  ''
