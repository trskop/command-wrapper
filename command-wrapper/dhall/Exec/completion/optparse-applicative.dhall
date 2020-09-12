-- vim: filetype=dhall
--
-- Command line interfaces built with
-- <https://hackage.haskell.org/package/optparse-applicative> library have
-- command line completion baked in.  This function understands its calling
-- convention.  Generated command looks like:
--
-- ```
-- COMMAND --bash-completion-index=INDEX [--bash-completion-enriched]
--     [--bash-completion-word=WORD ...]
-- ```

let ExecCommand =
      { Type =
            ../../CommandWrapper/ExecCommand/Type sha256:6ece797a2c269b469da41f12ec2d7206846cac65183ae8213cce1b6d59f2b02b
          ? ../../CommandWrapper/ExecCommand/Type
      , default =
            ../../CommandWrapper/ExecCommand/default sha256:c3a088ca2b090c91d5d630c4e01f4b4fbd0136f5bb1251f6828698e5180685b2
          ? ../../CommandWrapper/ExecCommand/default
      }

let Shell =
        ../../CommandWrapper/Shell/Type sha256:f61ef033bfb850ef4bf0c3d0d18c69d1b15b38cc9e3df4a1abea334b89ed5555
      ? ../../CommandWrapper/Shell/Type

let Prelude =
        ../../Prelude/package.dhall sha256:2086c6a2e7ce8ee371858036ab60d24d045969defadb4bc9d87639921ec2e028
      ? ../../Prelude/package.dhall

let enrichedCompletion =
      λ(shell : Shell) → merge { Bash = False, Fish = True, Zsh = True } shell

let completion =
      λ(command : Text) →
      λ(prefixArguments : List Text) →
      λ(shell : Shell) →
      λ(index : Natural) →
      λ(words : List Text) →
        ExecCommand::{
        , command
        , arguments =
            let adjustedIndex = index + Prelude.List.length Text prefixArguments

            in    [ "--bash-completion-index=${Natural/show adjustedIndex}" ]
                # ( if    enrichedCompletion shell
                    then  [ "--bash-completion-enriched" ]
                    else  [] : List Text
                  )
                # Prelude.List.map
                    Text
                    Text
                    (λ(word : Text) → "--bash-completion-word=${word}")
                    (prefixArguments # words)
        }

in    completion
    : ∀(command : Text) →
      ∀(prefixArguments : List Text) →
      Shell →
      ∀(index : Natural) →
      ∀(words : List Text) →
        ExecCommand.Type
