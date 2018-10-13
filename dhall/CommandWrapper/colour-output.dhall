let
    ColourOutput = ./Type/ColourOutput.dhall

in
    { always = <Always = {=} | Auto : {} | No : {} > : ColourOutput
    , auto = <Always : {} | Auto = {=} | No : {} > : ColourOutput
    , no = <Always : {} | Auto : {} | No = {=} > : ColourOutput
    }
