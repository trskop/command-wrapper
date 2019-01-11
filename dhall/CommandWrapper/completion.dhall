  λ(name : Text)
→ λ(toolset : Text)
→ λ(command : Text)
→ { bash =
      ''
      function _${name}()
      {
          COMPREPLY=($(COMMAND_WRAPPER_INVOKE_AS='${toolset}' "${command}" completion --index="''${COMP_CWORD}" --shell=bash -- "''${COMP_WORDS[@]}"))
      }
      complete -o filenames -F _${name} ${name}
      ''
  }
