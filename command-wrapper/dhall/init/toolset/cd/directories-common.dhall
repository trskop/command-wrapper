-- vim: filetype=dhall

λ(toolsetName : Text) →
λ(library : { prelude : Text, commandWrapper : Text, exec : Text }) →
λ(runtimeDirectory : { libDir : Text, manDir : Text }) →
  ''
  --let CommandWrapper =
  --      ${library.commandWrapper}

  --let Exec =
  --      ${library.exec}

  --let Prelude =
  --      ${library.prelude}

  let home = env:HOME as Text

  let config = env:XDG_CONFIG_HOME as Text ? "''${home}/.config"

  let local = "''${home}/.local"

  in    [ "''${config}/${toolsetName}"
        , "''${local}/lib/${toolsetName}"
        ]
      : List Text
  ''
