-- vim: filetype=dhall

let OutputColour =
        ./Type.dhall sha256:a7a3fce54d436216a0f504bfda5f3b67cb07c171209e9d904a9aa484507297fa
      ? ./Type.dhall

let toArguments =
      λ(_ : OutputColour) →
        merge
          { Colour = [ "--color-output" ]
          , Monochrome = [ "--monochrome-output" ]
          }
          _

let test0 = assert : toArguments OutputColour.Colour ≡ [ "--color-output" ]

in  toArguments : OutputColour → List Text
