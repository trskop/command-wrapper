-- vim: filetype=dhall

λ(toolsetName : Text) →
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

  let commands =
          ./exec/commands-common.dhall
        # (./exec/commands.dhall ? empty)
        # (./exec/commands-local.dhall ? empty)

  let global =
          ../command-wrapper/command-wrapper-exec.dhall
        ? CommandWrapper.ExecConfig::{=}

  in  global ⫽ { commands = global.commands # commands }
  ''
