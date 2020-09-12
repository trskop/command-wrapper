-- vim: filetype=dhall

let noArguments = [] : List Text

let optionalFlags =
      λ(enabled : List Text) →
      λ(disabled : List Text) →
      λ(flag : Optional Bool) →
        merge
          { None = noArguments
          , Some = λ(p : Bool) → if p then enabled else disabled
          }
          flag

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
