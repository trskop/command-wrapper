-- vim: filetype=dhall
--
-- Normally Command Wrapper toolset is just a symbolic link to Command Wrapper
-- executable.  As an alternative we may want to introduce a script instead of
-- symbolic link.
--
-- For more information on how this works see `command-wrapper(1)` manual page,
-- especially *ENVIRONMENT VARIABLES* section.
--
-- Usage example:
--
-- ```Bash
-- ${HOME}/.local/lib/command-wrapper config --dhall-text --output="${HOME}/.local/bin/command-wrapper" <<'EOF'
-- let T = ./toolset-invocation-script.dhall  -- This Dhall file.
--
-- let home = env:HOME as Text
--
-- in  T.template
--       T.Options::{
--       , commandWrapperExecutable = "${home}/.local/lib/command-wrapper"
--       , facadeExecutable = "${home}/.local/bin/command-wrapper"
--       , systemConfigDir = "${home}/.local/etc"
--       }
-- EOF
-- ```
--
-- See also `example0`, `example1`, and `example2` in this file.

let export =
      λ(name : Text) → λ(value : Text) → "${name}='${value}'; export ${name}"

let optionalExport =
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
      { Type =
          { commandWrapperExecutable : Text
          , facadeExecutable : Optional Text
          , toolsetName : Optional Text
          , systemConfigDir : Optional Text
          , userConfigDir : Optional Text
          }
      , default =
        { facadeExecutable = None Text
        , toolsetName = None Text
        , systemConfigDir = None Text
        , userConfigDir = None Text
        }
      }

let Options/consistency =
        assert
      :   Options::{ commandWrapperExecutable = "/some/executable" }
        ≡ (Options.default ⫽ { commandWrapperExecutable = "/some/executable" })

let defaultXdgConfigHome =
      env:XDG_CONFIG_HOME as Text ? "${env:HOME as Text}/.config"

let template =
        λ(options : Options.Type)
      → ''
        #!/bin/sh
        
        ${optionalExport "COMMAND_WRAPPER_FACADE" "" options.facadeExecutable}
        ${optionalExport
            "COMMAND_WRAPPER_SYSTEM_CONFIG_DIR"
            ""
            options.systemConfigDir}
        ${optionalExport
            "COMMAND_WRAPPER_USER_CONFIG_DIR"
            defaultXdgConfigHome
            options.userConfigDir}
        ${optionalExport "COMMAND_WRAPPER_INVOKE_AS" "" options.toolsetName}
        exec '${options.commandWrapperExecutable}' "$@"
        ''

let example0 =
        template Options::{ commandWrapperExecutable = "/some/executable" }
      ≡ ''
        #!/bin/sh
        
        # COMMAND_WRAPPER_FACADE='''; export COMMAND_WRAPPER_FACADE
        # COMMAND_WRAPPER_SYSTEM_CONFIG_DIR='''; export COMMAND_WRAPPER_SYSTEM_CONFIG_DIR
        # COMMAND_WRAPPER_USER_CONFIG_DIR='${defaultXdgConfigHome}'; export COMMAND_WRAPPER_USER_CONFIG_DIR
        # COMMAND_WRAPPER_INVOKE_AS='''; export COMMAND_WRAPPER_INVOKE_AS
        exec '/some/executable' "$@"
        ''

let example1 =
        template
          Options::{
          , commandWrapperExecutable = "/some/executable"
          , facadeExecutable = Some "/some/facade"
          , systemConfigDir = Some "/some/etc"
          }
      ≡ ''
        #!/bin/sh
        
        COMMAND_WRAPPER_FACADE='/some/facade'; export COMMAND_WRAPPER_FACADE
        COMMAND_WRAPPER_SYSTEM_CONFIG_DIR='/some/etc'; export COMMAND_WRAPPER_SYSTEM_CONFIG_DIR
        # COMMAND_WRAPPER_USER_CONFIG_DIR='${defaultXdgConfigHome}'; export COMMAND_WRAPPER_USER_CONFIG_DIR
        # COMMAND_WRAPPER_INVOKE_AS='''; export COMMAND_WRAPPER_INVOKE_AS
        exec '/some/executable' "$@"
        ''

let example2 =
        template
          Options::{
          , commandWrapperExecutable = "/some/executable"
          , facadeExecutable = Some "/some/facade"
          , toolsetName = Some "toolset"
          }
      ≡ ''
        #!/bin/sh
        
        COMMAND_WRAPPER_FACADE='/some/facade'; export COMMAND_WRAPPER_FACADE
        # COMMAND_WRAPPER_SYSTEM_CONFIG_DIR='''; export COMMAND_WRAPPER_SYSTEM_CONFIG_DIR
        # COMMAND_WRAPPER_USER_CONFIG_DIR='${defaultXdgConfigHome}'; export COMMAND_WRAPPER_USER_CONFIG_DIR
        COMMAND_WRAPPER_INVOKE_AS='toolset'; export COMMAND_WRAPPER_INVOKE_AS
        exec '/some/executable' "$@"
        ''

in  { Options, template }
