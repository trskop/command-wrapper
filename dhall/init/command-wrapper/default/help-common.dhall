-- vim: filetype=dhall

  λ(library : { commandWrapper : Text, exec : Text })
→ λ(runtimeDirectory : { libDir : Text, manDir : Text })
→ ''
  Global Subcommands:

    help       (internal, aliases: h, man)
    config     (internal, aliases: cfg, dhall, dhall-*)
    version    (internal)
    completion (internal)
    cd         (external)
    exec       (external)
    skel       (external)
  ''
