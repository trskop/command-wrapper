-- vim: filetype=dhall

λ(toolsetName : Text) →
λ(library : { prelude : Text, commandWrapper : Text, exec : Text }) →
λ(runtimeDirectory : { libDir : Text, manDir : Text }) →
  ''
  -- This file is intended to be under version control and shared among multiple
  -- systems.
  --
  -- Aliases that are ment to be available only on this specific machine should
  -- go into `./aliases-local.dhall`.  If local configuration is under version
  -- control then `./aliases-local.dhall` should be a symbolic link to that
  -- version controlled file, or it should contain an import of such file.  There
  -- is also `./aliases.dhall` which is intended to be used as a kind of staging
  -- environment, and it should not be under version control.

  let CommandWrapper =
        ${library.commandWrapper}

  --let Exec =
  --      ${library.exec}

  --let Prelude =
  --      ${library.prelude}

  let execConfig = ../command-wrapper-exec.dhall

  let SubcommandAlias = CommandWrapper.SubcommandAlias

  in    CommandWrapper.ExecNamedCommand.namedCommandsToAliases execConfig.commands
  --  # [ SubcommandAlias::{
  --      , alias = "cfg"
  --      , description = Some "TODO: I hereby promise to describe this command."
  --      , command = "config"
  --      }
  --    ]
  ''
