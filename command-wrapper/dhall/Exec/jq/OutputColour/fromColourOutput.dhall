-- vim: filetype=dhall

let OutputColour =
        ./Type.dhall sha256:a7a3fce54d436216a0f504bfda5f3b67cb07c171209e9d904a9aa484507297fa
      ? ./Type.dhall

let ColourOutput =
        ../../../CommandWrapper/ColourOutput/Type sha256:0ef91a3f044406ee80fc20f26127b448a0e98f46c46ec024457023d2aded8543
      ? ../../../CommandWrapper/ColourOutput/Type

let fromColourOutput =
      λ(_ : ColourOutput) →
        merge
          { Always = Some OutputColour.Colour
          , Auto = None OutputColour
          , Never = Some OutputColour.Monochrome
          }
          _

let test0 = assert : fromColourOutput ColourOutput.Auto ≡ None OutputColour

let test1 =
        assert
      : fromColourOutput ColourOutput.Never ≡ Some OutputColour.Monochrome

in  fromColourOutput : ColourOutput → Optional OutputColour
