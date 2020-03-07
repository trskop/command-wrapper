-- vim: filetype=dhall

{ bash =
      ./bash sha256:baec3569316a22139b4bbe2c135e54ba263b8278a6eff5b80335d476a7cb108e
    ? ./bash
, dhall =
      ./dhall sha256:8a78d4a15f202a63d130b48c141c1c4e9898faba19a1a1e8495bafbd0d43133a
    ? ./dhall
, haskell =
      ./haskell sha256:834ed5c4fa401bebb113d082a88d1b6479041cc1573467b58b251ced86493d1d
    ? ./haskell
}
