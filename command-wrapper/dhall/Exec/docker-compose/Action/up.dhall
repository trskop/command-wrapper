-- vim: filetype=dhall

let ColourOutput =
        ../../../CommandWrapper/ColourOutput/Type sha256:0ef91a3f044406ee80fc20f26127b448a0e98f46c46ec024457023d2aded8543
      ? ../../../CommandWrapper/ColourOutput/Type

let Action =
        ./Type.dhall sha256:e88f3cc3042912a2d9a24b75f93865227edbf368a70f5a8bb10f137abc8063c2
      ? ./Type.dhall

let UpOptions =
        ../UpOptions/Type.dhall sha256:ef326d47d9073f2c4b0ce34dfc14ea188551fac2170499087cbfacf2d5a73073
      ? ../UpOptions/Type.dhall

let up =
      λ(f : ColourOutput → UpOptions) →
        Some (λ(colourOutput : ColourOutput) → Action.Up (f colourOutput))

in  up : (ColourOutput → UpOptions) → Optional (ColourOutput → Action)