-- vim: filetype=dhall

{ command =
      ./command.dhall sha256:8af201477f8eb17da91110b0894b46699bf4c9eb31026d42ca5d3c9f3a2b81bd
    ? ./command.dhall
, completion-script =
      ./completion-script.dhall sha256:8cd0e788f0ec379cec17d226f2cf27b849865445aab56d0835487bf3a16eef50
    ? ./completion-script.dhall
, completion =
      ./completion.dhall sha256:6e9b58580442a7b58c9ea39780e75662fdae3171f649fd25d7f23c7c20321b28
    ? ./completion.dhall
}
