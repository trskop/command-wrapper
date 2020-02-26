-- vim: filetype=dhall

{ command =
      ./command sha256:5bca52c25012d10dbdcf2b8d866b434fbdf659eed458ec908385ca2c71896ef8
    ? ./command
, Options =
      ./Options/package.dhall sha256:8d86f12eb4d511a3b6c5f06ec2cd6bb193d7ec0a2e6950e271db9e4731240b4f
    ? ./Options/package.dhall
}
