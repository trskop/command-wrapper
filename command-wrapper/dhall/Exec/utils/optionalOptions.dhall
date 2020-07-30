-- vim: filetype=dhall
--
-- Convert an `Optional` value into command line options, or empty list if it's
-- `None`.

let Prelude =
        ../prelude.dhall sha256:10db3c919c25e9046833df897a8ffe2701dc390fa0893d958c3430524be5a43e
      ? ../prelude.dhall

let optionalOptions =
      λ(r : Type) →
      λ(just : r → List Text) →
      λ(optional : Optional r) →
        Prelude.Optional.fold r optional (List Text) just ([] : List Text)

let example =
        assert
      :   optionalOptions
            Text
            (λ(dir : Text) → [ "--directory=${dir}" ])
            (Some "/a/directory")
        ≡ [ "--directory=/a/directory" ]

in  optionalOptions : ∀(r : Type) → (r → List Text) → Optional r → List Text
