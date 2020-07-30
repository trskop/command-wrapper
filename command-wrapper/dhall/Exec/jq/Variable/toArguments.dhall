-- vim: filetype=dhall

let Value =
        ./Value.dhall sha256:0ce6022956239e8ae9922a49153dff9875b59929c881af9c2528db69f41ae074
      ? ./Value.dhall

let Variable =
        ./Type.dhall sha256:81af69a61b5774cc7c4c4902d08c60b0e88ddd62d28af37e346afb79a7e1aab1
      ? ./Type.dhall

let Prelude =
        ../../prelude.dhall sha256:10db3c919c25e9046833df897a8ffe2701dc390fa0893d958c3430524be5a43e
      ? ../../prelude.dhall

let toArguments =
      λ(_ : Variable) →
        merge
          { String = λ(text : Text) → [ "--arg", _.name, text ]
          , Json =
              λ(json : Prelude.JSON.Type) →
                [ "--argjson", _.name, Prelude.JSON.render json ]
          }
          _.value

let test0 =
        assert
      :   toArguments { name = "some", value = Value.String "text" }
        ≡ [ "--arg", "some", "text" ]

let test1 =
        assert
      :   toArguments
            { name = "flags"
            , value = Value.Json (Prelude.JSON.array [ Prelude.JSON.bool True ])
            }
        ≡ [ "--argjson"
          , "flags"
          , ''
            [ true ]
            ''
          ]

in  toArguments : Variable → List Text
