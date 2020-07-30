-- vim: filetype=dhall

{ Options =
      ./Options/package.dhall sha256:8ff31a81e6728a1fe6485eebedf31db89afbb738fb9644e9f3ae00e50a58247f
    ? ./Options/package.dhall
, command =
      ./command.dhall sha256:45cc2dfc9e61c3c4ca6495e19b9515efd4ce45333397acd02037a6f8e2bf0f86
    ? ./command.dhall
}
