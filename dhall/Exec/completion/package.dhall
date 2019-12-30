-- vim: filetype=dhall

{ command-wrapper =
      ./command-wrapper sha256:a0c07c4c13f59188a2fc22670b49e528c844a36486d94c6fb8de66949ae9b695
    ? ./command-wrapper
, optparse-applicative =
      ./optparse-applicative sha256:00d877d341515117d8f20b93d7119bd0342ece43d2202e01f470db73f162cb44
    ? ./optparse-applicative
, wordlist =
      ./wordlist sha256:dfc45aeb0300b5706c9090ce2af5595090218bc75e244af8fe7aa8d3fb00c6fa
    ? ./wordlist
, scripts =
      ./scripts/package.dhall sha256:afe58176b31fb9cbb98a8f0abf01a2c9561f1ec9567bbed1d72e5d785240e7bd
    ? ./scripts/package.dhall
, wrapper =
      ./wrapper sha256:2581cfe8c184898fe6b04aedad5ad9c1be34154b3a795ecab17ee19c899e5c8b
    ? ./wrapper
}
