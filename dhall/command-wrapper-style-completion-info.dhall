  λ(shell : < Bash | Fish | Zsh >)
→ λ(index : Natural)
→ λ(arguments : List Text)
→ let shellName = merge { Bash = "bash", Fish = "fish", Zsh = "zsh" } shell

  in    [ "--completion"
        , "--index=${Natural/show index}"
        , "--shell=${shellName}"
        , "--"
        ]
      # arguments
