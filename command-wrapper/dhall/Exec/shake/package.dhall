-- vim: filetype=dhall
--
-- TODO: Command-line completion

{ Options =
      ./Options/package.dhall sha256:0f997e41c6f1713a8c99f6af99fbdd67973de6651efa2d7464d5085168f19a1f
    ? ./Options/package.dhall
, CompactOutput =
      ./CompactOutput/package.dhall sha256:b461366e726abb1c388290a6110f2b790f06590e11520069f13d2bcadc585e9b
    ? ./CompactOutput/package.dhall
, ShakeVerbosity =
      ./ShakeVerbosity/package.dhall sha256:3d0bf0eede2a1cc535bb505f3980db0a6757565bf82891c161ce60cff11d4363
    ? ./ShakeVerbosity/package.dhall
, command =
      ./command sha256:68af3115d319bbfd3f2e045277ebd2b04dc94c82ec1027e7b76ee1ff16c9edc6
    ? ./command
}
