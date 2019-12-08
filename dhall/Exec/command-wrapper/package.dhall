-- vim: filetype=dhall

{ command =
      ./command sha256:46a22631407ef5c320c05a1b586f1eb67a1f59afab825e93d72063073a584172
    ? ./command
, completion =
      ./completion sha256:1cfce45487f963197ab82d9f90d93fbb91455a8beddf850b2d0b1f2228110342
    ? ./completion
, GlobalOptions =
      ./GlobalOptions/package.dhall sha256:ed4f1162592b84a06fe2803b4c24acc759635c55c8b9b24dc93c4b28f0867aeb
    ? ./GlobalOptions/package.dhall
}
