-- vim: filetype=dhall

{ optparse-applicative =
      ./optparse-applicative sha256:00d877d341515117d8f20b93d7119bd0342ece43d2202e01f470db73f162cb44
    ? ./optparse-applicative
, wordlist =
      ./wordlist sha256:19ba1f70e71d4b9bd9cd98b8fd43cfc74310c62d2916ddb65c37c4898044eb22
    ? ./wordlist
, scripts =
      ./scripts/package.dhall sha256:290571f2ceee834bee6dd6d3fbb24eb9fe9b323578e13ea3141f1dcccef3a297
    ? ./scripts/package.dhall
}
