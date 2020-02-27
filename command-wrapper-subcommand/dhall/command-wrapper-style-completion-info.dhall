-- vim: filetype=dhall
--
-- Dhall function that describes options for CommandWrapper-style command line
-- completion API.
--
-- ```
-- SOME_COMMAND --completion --index=INDEX --shell={bash|fish|zsh} -- [WORD ...]
-- ```

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
