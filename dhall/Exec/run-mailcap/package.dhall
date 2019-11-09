-- vim: filetype=dhall

{ command =
      ./command sha256:4d5369c2e96adca8785fe6ca441007125729aa5f914a4613ade6c56775cfa782
    ? ./command
, Action =
      ./Action/package.dhall sha256:8fe003e9f0c71f1e1f0c9d011864c217c0f050025da88414292b8394f9d00f8d
    ? ./Action/package.dhall
}
