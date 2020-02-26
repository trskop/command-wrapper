-- vim: filetype=dhall

  λ(toolsetName : Text )
→ λ(library : { commandWrapper : Text, exec : Text })
→ λ(runtimeDirectory : { libDir : Text, manDir : Text })
→ ''
  -- vim: filetype=dhall
  --
  -- This file is intended to be under version control and shared among multiple
  -- systems.
  --
  -- Aliases that are ment to be available only on this specific machine should
  -- go into `./aliases-local.dhall`.  If local configuration is under version
  -- control then `./aliases-local.dhall` should be a symbolic link to that
  -- version controlled file, or it should contain an import of such file.  There
  -- is also `./aliases.dhall` which is intended to be used as a kind of staging
  -- environment, and it should not be under version control.

  let CommandWrapper = ${library.commandWrapper}

  let execConfig = ../command-wrapper-exec.dhall

  let SubcommandAlias = CommandWrapper.SubcommandAlias

  in    CommandWrapper.ExecNamedCommand.namedCommandsToAliases execConfig.commands
      # [ SubcommandAlias::{
          , alias = "h"
          , description = Some "Shorthand for \"help\"."
          , command = "help"
          }
        ]
  ''
