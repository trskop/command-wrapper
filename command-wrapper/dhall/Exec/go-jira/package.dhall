-- vim: filetype=dhall

{ Options =
      ./Options/package.dhall sha256:af57e1763ede1a58d4690512a4fd5cb38a8ecd29db6803ba99a645de1b419869
    ? ./Options/package.dhall
, command =
      ./command sha256:70f772523190a0cad3cb091b8858fd9b2c9f7e20847da74e1b9a74f2f0cbf245
    ? ./command
, completion =
      ./completion sha256:5763e8d86ae8135ff57426ed4e4279e19ffb797f697c4fc610a2ea59d8efaf04
    ? ./completion
, completion-script =
      ./completion-script sha256:a2bed0ae3a53770a6d69132921baf9610199303beeefe7c84db0d45666d4ea2b
    ? ./completion-script
}
