-- vim: filetype=dhall
--
-- Dhall function that describes options for [`optparse-applicative`-style
-- ](https://hackage.haskell.org/package/optparse-applicative) command line
-- completion API.
--
-- ```
-- SOME_COMMAND --bash-compltion-index=INDEX [--bash-completion-word=WORD ...]
-- ```

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
