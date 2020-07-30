-- vim: filetype=dhall

{ Type =
      ./Type.dhall sha256:a7a3fce54d436216a0f504bfda5f3b67cb07c171209e9d904a9aa484507297fa
    ? ./Type.dhall
, toArguments =
      ./toArguments.dhall sha256:71ece86a9738de0ee56688255286128b4d7b513b15fbb41cdf8f06f6072465ca
    ? ./toArguments.dhall
, fromColourOutput =
      ./fromColourOutput.dhall sha256:b9c875bb20174ca0df962cb6d01550362c9bc68ef796cd013916c44a158eacd1
    ? ./fromColourOutput.dhall
}
