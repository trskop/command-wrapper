-- vim: filetype=dhall

let Value =
        ./Value.dhall sha256:0ce6022956239e8ae9922a49153dff9875b59929c881af9c2528db69f41ae074
      ? ./Value.dhall

let Variable = { name : Text, value : Value }

in  Variable : Type
