-- vim: filetype=dhall

  λ(library : { commandWrapper : Text, exec : Text })
→ λ(runtimeDirectory : { libDir : Text, manDir : Text })
→ ''
  -- vim: filetype=dhall

  let CommandWrapper = ${library.commandWrapper}

  let Exec = ${library.commandWrapper}

  let ExecNamedCommand = CommandWrapper.ExecNamedCommand

  let ExecCommand = CommandWrapper.ExecCommand

  in    [
  --      ExecNamedCommand::{
  --      , name = "echo"
  --      , description = Some "TODO: I hereby promise to describe this command."
  --      , command =
  --            λ(verbosity : CommandWrapper.Verbosity.Type)
  --          → λ(colourOutput : CommandWrapper.ColourOutput.Type)
  --          → λ(arguments : List Text)
  --          → ExecCommand::{ command = "echo", arguments = arguments }
  --      }
        ]
      : List CommandWrapper.ExecNamedCommand.Type
  ''
