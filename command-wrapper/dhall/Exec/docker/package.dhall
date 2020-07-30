-- vim: filetype=dhall

{ Options =
      ./Options/package.dhall sha256:b801edf6d8ea64eaad3d358bd788a378a58bd20a167cb8bde32d4fd51b9d1b72
    ? ./Options/package.dhall
, LogLevel =
      ./LogLevel/package.dhall sha256:f47b621e6f3d257ff2037ef1b59e1155b8c7f569f371b285ae8bf8169adff76d
    ? ./LogLevel/package.dhall
, ExecOptions =
      ./ExecOptions/package.dhall sha256:227af5c4f3ea7a7f05c560775e8ef911e8952f6ce796de762b8d57fe48e7c531
    ? ./ExecOptions/package.dhall
, RunOptions =
      ./RunOptions/package.dhall sha256:7898a8e98298633be152f27c022ad77b0707a9006caacd0a6a1b9bd2cd9c86ac
    ? ./RunOptions/package.dhall
, Environment =
      ./Environment/package.dhall sha256:a514274b0ed1d4135260eebbcc9ff6bae8078c0492f1cd54e4afab78cd8ef634
    ? ./Environment/package.dhall
, command =
      ./command.dhall sha256:058e3cb1f331261410ce12af3b037d76ced180788a86320432bf8d5f5bd6415c
    ? ./command.dhall
, prune =
      ./prune.dhall sha256:253e564b31e0e22b86fd47271f7f767a795bb38d00f71ca22ad1c3b6904e4f18
    ? ./prune.dhall
, exec =
      ./exec.dhall sha256:38345a0b12398a6d88478e95dd910195793d5a2c908aef5b79807ffe9f095ad6
    ? ./exec.dhall
, run =
      ./run.dhall sha256:24f39177af860effbc6b375da3d6b830ea351c188d5f542f7aa961dbb0bcec64
    ? ./run.dhall
}
