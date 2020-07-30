-- vim: filetype=dhall

{ command-wrapper =
      ./command-wrapper.dhall sha256:a0c07c4c13f59188a2fc22670b49e528c844a36486d94c6fb8de66949ae9b695
    ? ./command-wrapper.dhall
, optparse-applicative =
      ./optparse-applicative.dhall sha256:00d877d341515117d8f20b93d7119bd0342ece43d2202e01f470db73f162cb44
    ? ./optparse-applicative.dhall
, wordlist =
      ./wordlist.dhall sha256:efdcacf3484aa556c5d4419e63483fb364dcb855dacf96a691ccbfef80a4d154
    ? ./wordlist.dhall
, wrapper =
      ./wrapper.dhall sha256:2581cfe8c184898fe6b04aedad5ad9c1be34154b3a795ecab17ee19c899e5c8b
    ? ./wrapper.dhall
, bash-completion-script-wrapper =
      ./bash-completion-script-wrapper/package.dhall sha256:8f7fbe97c657d747d7cb0eb6de8d671cc37a9262196db66af236fc03f6ae2278
    ? ./bash-completion-script-wrapper/package.dhall
}
