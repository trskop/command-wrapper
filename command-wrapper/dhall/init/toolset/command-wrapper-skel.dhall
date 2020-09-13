-- vim: filetype=dhall

  λ(toolsetName : Text)
→ λ(library : { prelude : Text, commandWrapper : Text, exec : Text })
→ λ(runtimeDirectory : { libDir : Text, manDir : Text })
→ ''
  --let CommandWrapper =
  --      ${library.commandWrapper}

  --let Exec =
  --      ${library.exec}

  --let Prelude =
  --      ${library.prelude}

  let mkGlobal = ../command-wrapper/command-wrapper-skel.dhall

  in    λ(toolset : Text)
      → λ(subcommand : Text)
      → λ(command : Text)
      → let global = mkGlobal toolset subcommand command

        in    global
            ⫽ {=}
  ''
