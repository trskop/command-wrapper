-- vim: filetype=dhall

{ Type =
      ./Type.dhall sha256:81af69a61b5774cc7c4c4902d08c60b0e88ddd62d28af37e346afb79a7e1aab1
    ? ./Type.dhall
, Value =
      ./Value.dhall sha256:0ce6022956239e8ae9922a49153dff9875b59929c881af9c2528db69f41ae074
    ? ./Value.dhall
, toArguments =
      ./toArguments.dhall sha256:732827a49ffd90bed91b4b7306879c86fc0c1837f3da513372f888164943f211
    ? ./toArguments.dhall
}
