-- vim: filetype=dhall

  λ(library : { commandWrapper : Text, exec : Text })
→ λ(runtimeDirectory : { libDir : Text, manDir : Text })
→ ''
  -- vim: filetype=dhall
  --
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

  let CommandWrapper = ${library.commandWrapper}

  let SubcommandAlias = CommandWrapper.SubcommandAlias

  in  [ SubcommandAlias::{
        , alias = "h"
        , description = Some "Short hand for \"help\"."
        , command = "help"
        }

      , SubcommandAlias::{
        , alias = "man"
        , description = Some "Short hand for \"help --man\"."
        , command = "help"
        , arguments = [ "--man" ]
        }

      -- The advantage of having `cfg` as an alias for `config` is that it
      -- shares only one letter of its prefix with `completion`, which is
      -- useful when using command line completion.
      , SubcommandAlias::{
        , alias = "cfg"
        , description = Some "Short hand for \"config\"."
        , command = "config"
        }

      , SubcommandAlias::{
        , alias = "dhall"
        , description = Some "Short hand for \"config --dhall\"."
        , command = "config"
        , arguments = [ "--dhall" ]
        }

      , SubcommandAlias::{
        , alias = "dhall-repl"
        , description = Some "Short hand for \"config --dhall-repl\"."
        , command = "config"
        , arguments = [ "--dhall-repl" ]
        }
      ]
  ''
