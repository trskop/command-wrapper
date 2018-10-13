let
    ColourOutput = ./Type/ColourOutput.dhall

in let
    fold
      : ∀(r : Type)
      → ∀ (handler
            : { Always : {} → r
              , Auto : {} → r
              , Never : {} → r
              }
          )
      → ∀(colourOutput : ColourOutput)
      → r

      = λ(r : Type)
      → λ(handler
            : { Always : {} → r
              , Auto : {} → r
              , Never : {} → r
              }
          )
      → λ(colourOutput : ColourOutput)
      → merge handler colourOutput

in let
    toText = fold Text
      { Always = λ(_ : {}) → "always"
      , Auto = λ(_ : {}) → "auto"
      , Never = λ(_ : {}) → "never"
      }

in
    { always = <Always = {=} | Auto : {} | Never : {} > : ColourOutput
    , auto = <Always : {} | Auto = {=} | Never : {} > : ColourOutput
    , never = <Always : {} | Auto : {} | Never = {=} > : ColourOutput
    , fold = fold
    , toText = toText
    }
