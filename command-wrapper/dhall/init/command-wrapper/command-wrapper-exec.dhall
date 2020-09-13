-- vim: filetype=dhall

λ(library : { prelude : Text, commandWrapper : Text, exec : Text }) →
λ(runtimeDirectory : { libDir : Text, manDir : Text }) →
  ''
  let CommandWrapper =
        ${library.commandWrapper}

  --let Exec =
  --      ${library.exec}

  --let Prelude =
  --      ${library.prelude}

  let empty = [] : List CommandWrapper.ExecNamedCommand.Type

  in  CommandWrapper.ExecConfig::{
      , commands =
            ./exec/commands-common.dhall
          # (./exec/commands.dhall ? empty)
          # (./exec/commands-local.dhall ? empty)
      }
  ''
