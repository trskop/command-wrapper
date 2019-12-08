-- vim: filetype=dhall
--
-- Library of utilities for Command Wrapper's `exec` subcommand.

{ bazel =
      ./bazel/package.dhall sha256:a18d34414126e78b7995493492d33c6cea6210d4a4a156025a818f3da262eea7
    ? ./bazel/package.dhall
, buildifier =
      ./buildifier/package.dhall sha256:2db5e3723ff4a17092c826b9d9200eab106d720dd940c7b4024d5a847e68f404
    ? ./buildifier/package.dhall
, command-wrapper =
      ./command-wrapper/package.dhall sha256:196834fd31413e78d5f8e28d6dfec488e74b8bf21f7e90c20d610bdca3c880bb
    ? ./command-wrapper/package.dhall
, completion =
      ./completion/package.dhall sha256:9df2a7d9aa206619ad3fc72bedd1f790f3944ceb7ec16d2611c163eaf02753b5
    ? ./completion/package.dhall
, direnv =
      ./direnv/package.dhall sha256:4cd1eb2a57bc33bd178772417c8553199f002458e42fe9524ebe688e0d71397d
    ? ./direnv/package.dhall
, docker =
      ./docker/package.dhall sha256:13ee98abc91fa54c5b43d77d1d855c6fb4776cff0daaca961fb3de451bb95740
    ? ./docker/package.dhall
, docker-compose =
      ./docker-compose/package.dhall sha256:6afaf8e2718377264e4633db2aeea51c655692404a6a2d16bc4e4f63c1ca9c58
    ? ./docker-compose/package.dhall
, firefox =
      ./firefox/package.dhall sha256:7f6e5fe7087c2dbc297349b4640c9a5a1e04ebe378254fedb254e9a7b2c94140
    ? ./firefox/package.dhall
, fzf =
      ./fzf/package.dhall sha256:e86a0b487344a4070397fb0a6bcdff4a57b74451f8673836f73981c33124f976
    ? ./fzf/package.dhall
, fzy =
      ./fzy/package.dhall sha256:41e967ece673a9681c2ace686f8ff2579c532c9c11fb595294ee0e70ce2b77b2
    ? ./fzy/package.dhall
, go-jira =
      ./go-jira/package.dhall sha256:d4deec8aebedd14031af8623c947727888784ae426c2786bac122de3354776ce
    ? ./go-jira/package.dhall
, jq =
      ./jq/package.dhall sha256:963c5dfa15b7686bd03fe07aae7c6ca1198994bf6e0e07edbaa29bf330d40d0e
    ? ./jq/package.dhall
, nix =
      ./nix/package.dhall sha256:a816e103f072359f0a9a6fc769223796a69de09d4413245ab691075cc207e155
    ? ./nix/package.dhall
, pg_dump =
      ./pg_dump/package.dhall sha256:0c0e9c664275c44fe289adf4972a0c8e16afc8d72238ab3c7eb449fa4a479628
    ? ./pg_dump/package.dhall
, psql =
      ./psql/package.dhall sha256:be18fe8eec49904ca79689ecbda31ec29b7fd26e12fc562a25ab9f5550b4fa76
    ? ./psql/package.dhall
, run-mailcap =
      ./run-mailcap/package.dhall sha256:86ae21c3007712c87f519297d036135c33c9bf23bd89008b8134ee1907940d88
    ? ./run-mailcap/package.dhall
, ssh =
      ./ssh/package.dhall sha256:14a415e7a977a3489c9f6d9feee7c1af32b385ad84be575215cf9f2199d6e83f
    ? ./ssh/package.dhall
, stack =
      ./stack/package.dhall sha256:e5cfa316b602f98440af0b12de51af2ea208009feb32af6f7ed615edecd31669
    ? ./stack/package.dhall
, tmux =
      ./tmux/package.dhall sha256:6853ffde06afb909d864a52ffc1b425c81ce2c99d28aa4fb3913a01e102ce3a5
    ? ./tmux/package.dhall
, utils =
      ./utils/package.dhall sha256:6829062ffbbb32dd9382a5a79b6f184656d95a0ff6e75c0dcfb8ccf090109269
    ? ./utils/package.dhall
, xdg-open =
      ./xdg-open/package.dhall sha256:141cea3791cf64099e76ae3245f60c248c386fce353c68d410d297af2d4191b8
    ? ./xdg-open/package.dhall
, yarn =
      ./yarn/package.dhall sha256:7c8b77114dbe143fa4143aee185b1ee25fa6d7b0a4e8b799030464300a5862bd
    ? ./yarn/package.dhall
, youtube-dl =
      ./youtube-dl/package.dhall sha256:1f740feb774b344ba601bb06fcaa2af73b67066b7a826eccf0a9f6af94792afc
    ? ./youtube-dl/package.dhall
}
