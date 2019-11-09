-- vim: filetype=dhall

{ Options =
      ./Options/package.dhall sha256:af57e1763ede1a58d4690512a4fd5cb38a8ecd29db6803ba99a645de1b419869
    ? ./Options/package.dhall
, command =
      ./command sha256:70f772523190a0cad3cb091b8858fd9b2c9f7e20847da74e1b9a74f2f0cbf245
    ? ./command
, completion =
      ./completion sha256:292135ca62edbe2505fd98b39bfe2bf3eab510f1bec837cc94dd2774ba0bd275
    ? ./completion
}
