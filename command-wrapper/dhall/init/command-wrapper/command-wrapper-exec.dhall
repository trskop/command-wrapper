-- vim: filetype=dhall

  λ(library : { commandWrapper : Text, exec : Text })
→ λ(runtimeDirectory : { libDir : Text, manDir : Text })
→ ''
  -- vim: filetype=dhall

  let CommandWrapper = ${library.commandWrapper}

  -- let Exec = ${library.exec}

  let empty = [] : List CommandWrapper.ExecNamedCommand.Type

  in  CommandWrapper.ExecConfig::{
      , commands =
            ./exec/commands-common.dhall
          # (./exec/commands.dhall ? empty)
          # (./exec/commands-local.dhall ? empty)
      }
  ''
