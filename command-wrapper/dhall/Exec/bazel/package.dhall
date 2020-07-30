-- vim: filetype=dhall

{ command =
      ./command.dhall sha256:8af201477f8eb17da91110b0894b46699bf4c9eb31026d42ca5d3c9f3a2b81bd
    ? ./command.dhall
, completion-script =
      ./completion-script.dhall sha256:732b498c252fe00f55da10780ca9b31ae0b56237741c97e778313556ec26e027
    ? ./completion-script.dhall
, completion =
      ./completion.dhall sha256:a4e0533e0e4003b61c14a8bd52b55ce53289159ffc2df065489dd0d7c760c698
    ? ./completion.dhall
}
