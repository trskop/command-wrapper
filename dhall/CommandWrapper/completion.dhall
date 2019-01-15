  λ(name : Text)
→ λ(toolset : Text)
→ λ(command : Text)
→ { bash =
      ''
      function _${name}()
      {
          COMP_WORDBREAKS="''${COMP_WORDBREAKS//=}"
          mapfile -t COMPREPLY < <(COMMAND_WRAPPER_INVOKE_AS='${toolset}' "${command}" completion --index="''${COMP_CWORD}" --shell=bash -- "''${COMP_WORDS[@]}")
          [[ "''${COMPREPLY[0]}" == "''${COMPREPLY[0]%=}=" ]] && compopt -o nospace
      }
      complete -o filenames -F _${name} ${name}
      ''
  }
