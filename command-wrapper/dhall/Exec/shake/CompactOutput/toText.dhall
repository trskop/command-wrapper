-- vim: filetype=dhall

let CompactOutput =
        ./Type.dhall sha256:3f7e82ca4628384d6df93a0c6b8813d1d38a438b096695cb219c24ffae9cc87b
      ? ./Type.dhall

let toText =
      λ(_ : CompactOutput) → merge { Yes = "yes", No = "no", Auto = "auto" } _

in  toText : CompactOutput → Text
