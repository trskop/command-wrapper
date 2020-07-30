-- vim: filetype=dhall

{ Options =
      ./Options/package.dhall sha256:74793f048c90cdfa08280f490b20fa7a147dfbc724f45cd9df8aafe75f470320
    ? ./Options/package.dhall
, command =
      ./command.dhall sha256:b3b512751851a2284267d7abc828aa26dda979b9cfd258a4a8f035853978e9b8
    ? ./command.dhall
}
