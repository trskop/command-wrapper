-- vim: filetype=dhall

{ Type =
      ./Type.dhall sha256:6ab5e997b4b51ec84ec2c002c400f173d60e3863e088bf2c2b92028a39196825
    ? ./Type.dhall
, default =
      ./default.dhall sha256:6814645e2843e24ffa24c18c1abb7f63bded053fba95a7e2b2e0949f8f41e21d
    ? ./default.dhall
, toEnvironment =
      ./toEnvironment.dhall sha256:caf74fb941a4c8c26268fc27a37744de75d1f8d753a7942d3f8b4aa2fe0dce1a
    ? ./toEnvironment.dhall
}
