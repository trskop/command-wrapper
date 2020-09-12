-- vim: filetype=dhall
--
-- Convert an `Optional` value into command line options, or empty list if it's
-- `None`.

let optionalOptions =
      λ(r : Type) →
      λ(just : r → List Text) →
      λ(optional : Optional r) →
        merge { None = [] : List Text, Some = just } optional

let example =
        assert
      :   optionalOptions
            Text
            (λ(dir : Text) → [ "--directory=${dir}" ])
            (Some "/a/directory")
        ≡ [ "--directory=/a/directory" ]

in  optionalOptions : ∀(r : Type) → (r → List Text) → Optional r → List Text
