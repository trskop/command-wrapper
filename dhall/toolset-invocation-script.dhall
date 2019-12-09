-- vim: filetype=dhall
--
-- Normally Command Wrapper toolset is just a symbolic link to Command Wrapper
-- executable.  As an alternative we may want to introduce a script instead of
-- symbolic link.  This file is a Dhall function that generates such script.

let export = λ(name : Text) → λ(value : Text) → "export ${name}='${value}'"

let Optional/export =
        λ(name : Text)
      → λ(default : Text)
      → λ(value : Optional Text)
      → Optional/fold
          Text
          value
          Text
          (export name)
          ("# " ++ export name default)

let Options =
      { commandWrapperExecutable : Text
      , toolsetName : Text
      , configDir : Optional Text
      , noColour : Bool
      , path : Optional Text
      , manPath : Optional Text
      }

let home = env:HOME as Text

let defaultXdgConfigHome = env:XDG_CONFIG_HOME as Text ? "${home}/.config"

let defaultPath =
        λ(toolsetName : Text)
      → "${home}/.local/lib/command-wrapper:${home}/.local/lib/${toolsetName}"

let defaultManPath = "${env:XDG_DATA_HOME as Text ? "${home}/.local/share"}/man"

in    λ(options : Options)
    → ''
      #!/bin/sh

      # How Command Wrapper uses following environment variables is described
      # in `command-wrapper(1)`.
      ${Optional/export
          "XDG_CONFIG_HOME"
          defaultXdgConfigHome
          options.configDir}
      ${Optional/export
          "NO_COLOR"
          "true"
          (if options.noColour then Some "true" else None Text)}
      ${Optional/export
          "COMMAND_WRAPPER_PATH"
          (defaultPath options.toolsetName)
          options.path}
      ${Optional/export
          "COMMAND_WRAPPER_MANPATH"
          defaultManPath
          options.manPath}
      ${export "COMMAND_WRAPPER_INVOKE_AS" options.toolsetName}

      exec '${options.commandWrapperExecutable}' "$@"
      ''
