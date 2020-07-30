-- vim: filetype=dhall

let Prelude =
        ../prelude.dhall sha256:10db3c919c25e9046833df897a8ffe2701dc390fa0893d958c3430524be5a43e
      ? ../prelude.dhall

let noArguments = [] : List Text

let optionalFlags =
      λ(enabled : List Text) →
      λ(disabled : List Text) →
      λ(flag : Optional Bool) →
        Prelude.Optional.fold
          Bool
          flag
          (List Text)
          (λ(p : Bool) → if p then enabled else disabled)
          noArguments

let test0 =
        assert
      : optionalFlags [ "--thing" ] [ "--no-thing" ] (None Bool) ≡ noArguments

let test1 =
        assert
      : optionalFlags [ "--thing" ] [ "--no-thing" ] (Some True) ≡ [ "--thing" ]

let test2 =
        assert
      :   optionalFlags [ "--thing" ] [ "--no-thing" ] (Some False)
        ≡ [ "--no-thing" ]

in    optionalFlags
    : ∀(enabled : List Text) →
      ∀(disabled : List Text) →
      Optional Bool →
        List Text
