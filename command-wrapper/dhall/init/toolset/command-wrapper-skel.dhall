-- vim: filetype=dhall

  λ(toolsetName : Text)
→ λ(library : { commandWrapper : Text, exec : Text })
→ λ(runtimeDirectory : { libDir : Text, manDir : Text })
→ ''
  -- vim: filetype=dhall

  let CommandWrapper = ${library.commandWrapper}

  let mkGlobal = ../command-wrapper/command-wrapper-skel.dhall

  in    λ(toolset : Text)
      → λ(subcommand : Text)
      → λ(command : Text)
      → let global = mkGlobal toolset subcommand command

        in    global
            ⫽ {=}
  ''
