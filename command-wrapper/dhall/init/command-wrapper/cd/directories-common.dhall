-- vim: filetype=dhall

  λ(library : { commandWrapper : Text, exec : Text })
→ λ(runtimeDirectory : { libDir : Text, manDir : Text })
→ ''
  -- vim: filetype=dhall

  let home = env:HOME as Text

  let config = env:XDG_CONFIG_HOME as Text ? "''${home}/.config"

  let local = "''${home}/.local"

  in    [ "''${config}"
        , "''${config}/command-wrapper"
        , "''${local}/lib/command-wrapper"
        , "''${home}/Downloads"
        , "''${home}/.ssh"
        ]
      : List Text
  ''
