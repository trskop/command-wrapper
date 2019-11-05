-- vim: filetype=dhall

  λ(toolsetName : Text)
→ λ(library : { commandWrapper : Text, exec : Text })
→ λ(runtimeDirectory : { libDir : Text, manDir : Text })
→ ''
  -- vim: filetype=dhall

  let CommandWrapper = ${library.commandWrapper}

  -- let Exec = ${library.exec}

  let global = ../command-wrapper/command-wrapper-exec.dhall

  let empty = [] : List CommandWrapper.ExecNamedCommand.Type

  in    global
      ⫽ { commands =
              global.commands
            # ./exec/commands-common.dhall
            # (./exec/commands.dhall ? empty)
            # (./exec/commands-local.dhall ? empty)
        }
  ''
