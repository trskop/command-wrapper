-- vim: filetype=dhall

let Prelude =
        ../../prelude.dhall sha256:10db3c919c25e9046833df897a8ffe2701dc390fa0893d958c3430524be5a43e
      ? ../../prelude.dhall

let Value = < String : Text | Json : Prelude.JSON.Type >

in  Value : Type
