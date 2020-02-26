-- vim: filetype=dhall

{ command-wrapper =
      ./command-wrapper sha256:a0c07c4c13f59188a2fc22670b49e528c844a36486d94c6fb8de66949ae9b695
    ? ./command-wrapper
, optparse-applicative =
      ./optparse-applicative sha256:00d877d341515117d8f20b93d7119bd0342ece43d2202e01f470db73f162cb44
    ? ./optparse-applicative
, wordlist =
      ./wordlist sha256:6984c0f59b61ea8feed634bde8ced6f88fed27ecdf2fe89ac4467d626c370a8b
    ? ./wordlist
, scripts =
      ./scripts/package.dhall sha256:afe58176b31fb9cbb98a8f0abf01a2c9561f1ec9567bbed1d72e5d785240e7bd
    ? ./scripts/package.dhall
, wrapper =
      ./wrapper sha256:2581cfe8c184898fe6b04aedad5ad9c1be34154b3a795ecab17ee19c899e5c8b
    ? ./wrapper
, bash-completion-script-wrapper =
      ./bash-completion-script-wrapper/package.dhall sha256:ef8edc2d55aae2dfa91e8d7aa858befd1d8f107bcb2cc260eb26bc0be840c46d
    ? ./bash-completion-script-wrapper/package.dhall
}
