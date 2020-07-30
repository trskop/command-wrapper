-- vim: filetype=dhall

let Environment =
        ../../CommandWrapper/Environment/package.dhall sha256:fd6671bdf4aec8a9bf066dd15cd0b97c68d2b6d6170676aed7ce1248f9533db1
      ? ../../CommandWrapper/Environment/package.dhall

let Prelude =
        ../prelude.dhall sha256:10db3c919c25e9046833df897a8ffe2701dc390fa0893d958c3430524be5a43e
      ? ../prelude.dhall

let optionalEnvironmentVariables
    : ∀(r : Type) → (r → Environment.Type) → Optional r → Environment.Type
    = λ(r : Type) →
      λ(just : r → Environment.Type) →
      λ(optional : Optional r) →
        Prelude.Optional.fold r optional Environment.Type just Environment.empty

let property0 =
      λ(v : Text) →
          assert
        :   optionalEnvironmentVariables
              Text
              (λ(_ : Text) → [ { name = "SOME_ENV_VARIABLE", value = _ } ])
              (Some v)
          ≡ [ { name = "SOME_ENV_VARIABLE", value = v } ]

let test0 =
        assert
      :   optionalEnvironmentVariables
            Text
            (λ(_ : Text) → [ { name = "SOME_ENV_VARIABLE", value = _ } ])
            (None Text)
        ≡ Environment.empty

in    optionalEnvironmentVariables
    : ∀(r : Type) → (r → Environment.Type) → Optional r → Environment.Type
