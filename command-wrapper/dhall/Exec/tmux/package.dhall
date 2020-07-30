-- vim: filetype=dhall

{ Options =
      ./Options/package.dhall sha256:261163963897b080dd24ee84b86bd79f3f7fb36c3a7d3d856d2bc9f7118b5425
    ? ./Options/package.dhall
, command =
      ./command.dhall sha256:743f09547fa19044a9c6059515cab6d9ac7cc9cc44efc6dbaae82b36f6653b0a
    ? ./command.dhall
}
