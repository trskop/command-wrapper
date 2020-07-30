-- vim: filetype=dhall

let Action =
        ./Type.dhall sha256:ebe44424cf93ffc0e67bbc651e5fea563e1f88a5b9a2bac59589adb0228b193d
      ? ./Type.dhall

let toArguments =
      λ(action : Action) →
          [     "--action="
            ++  merge
                  { View = "view"
                  , Cat = "cat"
                  , Compose = "compose"
                  , Composetyped = "composetyped"
                  , Edit = "edit"
                  , Print = "print"
                  }
                  action
          ]
        : List Text

in  toArguments : ∀(action : Action) → List Text
