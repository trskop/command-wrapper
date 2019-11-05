-- vim: filetype=dhall

  λ(toolsetName : Text )
→ λ(library : { commandWrapper : Text, exec : Text })
→ λ(runtimeDirectory : { libDir : Text, manDir : Text })
→ ''
  # Configuration for Command Wrapper toolset ${toolsetName}

  Custom toolset built using Command Wrapper.  Online documentation is available
  on [github.com/trskop/command-wrapper
  ](https://github.com/trskop/command-wrapper).
  ''
