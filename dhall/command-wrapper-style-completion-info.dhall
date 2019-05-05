  λ(shell : < Bash : {} | Fish : {} | Zsh : {} >)
→ λ(index : Natural)
→ λ(arguments : List Text)
→ let shellName =
        merge
        { Bash =
            λ(_ : {}) → "bash"
        , Fish =
            λ(_ : {}) → "fish"
        , Zsh =
            λ(_ : {}) → "zsh"
        }
        shell

  in    [ "--index=${Natural/show index}", "--shell=${shellName}", "--" ]
      # arguments
