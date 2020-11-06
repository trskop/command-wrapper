-- vim: filetype=dhall

let ColourOutput =
        ../../../CommandWrapper/ColourOutput/Type sha256:0ef91a3f044406ee80fc20f26127b448a0e98f46c46ec024457023d2aded8543
      ? ../../../CommandWrapper/ColourOutput/Type

let Action =
        ./Type.dhall sha256:e88f3cc3042912a2d9a24b75f93865227edbf368a70f5a8bb10f137abc8063c2
      ? ./Type.dhall

let DownOptions =
        ../DownOptions/Type.dhall sha256:760e4cd0b9b14aa5c3225f155da55eb8eef9c41a75a4535665ac5d7c6aceb182
      ? ../DownOptions/Type.dhall

let down =
      λ(downOptions : DownOptions) →
        Some (λ(_ : ColourOutput) → Action.Down downOptions)

in  down : DownOptions → Optional (ColourOutput → Action)