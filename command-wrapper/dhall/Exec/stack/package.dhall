-- vim: filetype=dhall

{ Options =
      ./Options/package.dhall sha256:e0882fee448fbf17d5e9324aa4ca5513cbc504318b1db81e38a28d8070ebd039
    ? ./Options/package.dhall
, command =
      ./command.dhall sha256:4796f0524e6a3ed15fcaceeb285d00cda61b910b0cb9540f024570ca9b0a71f2
    ? ./command.dhall
, completion =
      ./completion.dhall sha256:abd17e03d89a82e5f94350595ffdbd2e63e277d56c4d819b76cd5a9baa5ad28e
    ? ./completion.dhall
}
