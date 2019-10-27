-- vim: filetype=dhall

{ command-wrapper =
      ./command-wrapper sha256:a0c07c4c13f59188a2fc22670b49e528c844a36486d94c6fb8de66949ae9b695
    ? ./command-wrapper
, optparse-applicative =
      ./optparse-applicative sha256:00d877d341515117d8f20b93d7119bd0342ece43d2202e01f470db73f162cb44
    ? ./optparse-applicative
, wordlist =
      ./wordlist sha256:19ba1f70e71d4b9bd9cd98b8fd43cfc74310c62d2916ddb65c37c4898044eb22
    ? ./wordlist
, scripts =
      ./scripts/package.dhall sha256:7217882ec202f023042b88e65349216111697ebecacde87dc2a910cf3634eb29
    ? ./scripts/package.dhall
, wrapper =
      ./wrapper sha256:6f4115a040ee81275a40a8785baefbe391cf67cb57e5d4e79a8e0fedf572c036
    ? ./wrapper
}
