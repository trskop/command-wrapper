  λ(_ : < Bash | Fish | Zsh >)
→ λ(index : Natural)
→ λ(arguments : List Text)
→   [ "--bash-completion-index=${Natural/show index}" ]
  # List/fold
    Text
    arguments
    (List Text)
    (   λ(arg : Text)
      → λ(rest : List Text)
      → [ "--bash-completion-word=${arg}" ] # rest
    )
    ([] : List Text)
