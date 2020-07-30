-- vim: filetype=dhall
--
-- TODO: Command-line completion

{ Options =
      ./Options/package.dhall sha256:624ef494d005346cadebdf9c5c6600e2a35e5f1d4c51123437d938cfc9359c49
    ? ./Options/package.dhall
, CompactOutput =
      ./CompactOutput/package.dhall sha256:b461366e726abb1c388290a6110f2b790f06590e11520069f13d2bcadc585e9b
    ? ./CompactOutput/package.dhall
, ShakeVerbosity =
      ./ShakeVerbosity/package.dhall sha256:3d0bf0eede2a1cc535bb505f3980db0a6757565bf82891c161ce60cff11d4363
    ? ./ShakeVerbosity/package.dhall
, command =
      ./command.dhall sha256:37776d180763824e6e77898f15f94e35b3f717b63eed621eacffb3bb11cd6dd2
    ? ./command.dhall
}
