-- vim: filetype=dhall

{ Options =
      ./Options/package.dhall sha256:af57e1763ede1a58d4690512a4fd5cb38a8ecd29db6803ba99a645de1b419869
    ? ./Options/package.dhall
, command =
      ./command sha256:70f772523190a0cad3cb091b8858fd9b2c9f7e20847da74e1b9a74f2f0cbf245
    ? ./command
, completion =
      ./completion sha256:3356334155dfe42daa4bb50a10b1aef090daf6512cbd84fa86954c8326b24707
    ? ./completion
}
