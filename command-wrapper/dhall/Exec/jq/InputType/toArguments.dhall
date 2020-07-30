-- vim: filetype=dhall

let InputType =
        ./Type.dhall sha256:6d1a56a4bc24cfca6a6470a1e2c781b2092c60b64aff6cc19a341827e447e1a9
      ? ./Type.dhall

let toArguments =
      λ(_ : InputType) →
        merge { Raw = [ "--raw-input" ], Null = [ "--null-input" ] } _

let test0 = assert : toArguments InputType.Raw ≡ [ "--raw-input" ]

in  toArguments : InputType → List Text
