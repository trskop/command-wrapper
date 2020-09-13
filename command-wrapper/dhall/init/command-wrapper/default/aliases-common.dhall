-- vim: filetype=dhall

λ(library : { prelude : Text, commandWrapper : Text, exec : Text }) →
λ(runtimeDirectory : { libDir : Text, manDir : Text }) →
  ''
  -- This file is intended to be under version control and shared among multiple
  -- systems. It defines aliases that should be available everywhere and via all
  -- toolsets.
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

  let Prelude =
        ${library.prelude}

  let SubcommandAlias = CommandWrapper.SubcommandAlias

  let dhallAliases =
        Prelude.List.map
          Text
          CommandWrapper.SubcommandAlias.Type
          ( λ(_ : Text) →
              SubcommandAlias::{
              , alias = "dhall''${_}"
              , description = Some "Shorthand for \"config --dhall''${_}\"."
              , command = "config"
              , arguments = [ "--dhall''${_}" ]
              }
          )
          [ ""
          , "-bash"
          , "-diff"
          , "-exec"
          , "-filter"
          , "-format"
          , "-freeze"
          , "-hash"
          , "-lint"
          , "-repl"
          , "-resolve"
          , "-text"
          ]

  in    [ SubcommandAlias::{
          , alias = "h"
          , description = Some "Shorthand for \"help\"."
          , command = "help"
          }
        , SubcommandAlias::{
          , alias = "man"
          , description = Some "Shorthand for \"help --man\"."
          , command = "help"
          , arguments = [ "--man" ]
          }
        ]
      # dhallAliases
  ''
