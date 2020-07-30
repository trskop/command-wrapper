-- vim: filetype=dhall

{ Options =
      ./Options/package.dhall sha256:f40d104b121ea8568116570f0eebafc8853c7983ef3e9efcd40ad9326a92e481
    ? ./Options/package.dhall
, Command =
      ./Command/package.dhall sha256:59105ba8b28630c540c527be9ee60b8f5408a806c4b0689163280dc7a00f916d
    ? ./Command/package.dhall
, command =
      ./command.dhall sha256:bc72183de2c2403241545437301b64b63d25585ed83bdab73e4af14d70dfc59a
    ? ./command.dhall
, completion =
      ./completion.dhall sha256:3da408289aa9d82d0e2dd219063232acfc5e893f15269dec3270f85241013824
    ? ./completion.dhall
, completion-script =
      ./completion-script.dhall sha256:4a79f8b86f6ace664d44b60d10e6b1494a64dd339e071979fd3bcbf4d73d9e1c
    ? ./completion-script.dhall
}
