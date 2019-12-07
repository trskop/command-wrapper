-- vim: filetype=dhall
--
-- Library of utilities for Command Wrapper's `exec` subcommand.

{ bazel =
      ./bazel/package.dhall sha256:33dbe244bfb156bb5e6e341dec0fcbbea0cc8e586573e5248aa45a83a171eb53
    ? ./bazel/package.dhall
, buildifier =
      ./buildifier/package.dhall sha256:2db5e3723ff4a17092c826b9d9200eab106d720dd940c7b4024d5a847e68f404
    ? ./buildifier/package.dhall
, completion =
      ./completion/package.dhall sha256:7f07fe8f925c1c23162cb10033c2a356cf2910f7cb2eabc97a67bc44b6f669ec
    ? ./completion/package.dhall
, direnv =
      ./direnv/package.dhall sha256:458b34afebbead5075e9b9bd9e6cb9eb9928f6cab98d09e1f0bbe927fe7e1e69
    ? ./direnv/package.dhall
, docker =
      ./docker/package.dhall sha256:13ee98abc91fa54c5b43d77d1d855c6fb4776cff0daaca961fb3de451bb95740
    ? ./docker/package.dhall
, docker-compose =
      ./docker-compose/package.dhall sha256:b00c6d6249d369baa50990bf3addfa8f9330e2f1b75bfd3bc9e99720cae94acf
    ? ./docker-compose/package.dhall
, firefox =
      ./firefox/package.dhall sha256:7f6e5fe7087c2dbc297349b4640c9a5a1e04ebe378254fedb254e9a7b2c94140
    ? ./firefox/package.dhall
, fzy =
      ./fzy/package.dhall sha256:41e967ece673a9681c2ace686f8ff2579c532c9c11fb595294ee0e70ce2b77b2
    ? ./fzy/package.dhall
, go-jira =
      ./go-jira/package.dhall sha256:4d68e000b953e8f29119a571bf945f02b1530e083e8286a8c9c56c7ee87bf42a
    ? ./go-jira/package.dhall
, jq =
      ./jq/package.dhall sha256:ebc698204042482aae847495990f9dec0f24fa6c3cf06848cf60ab83154c1451
    ? ./jq/package.dhall
, nix =
      ./nix/package.dhall sha256:527303c982e212ac38a437fb544d1ae652d69ea0fddd8799be6099d26d6368ec
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
      ./utils/package.dhall sha256:2affcf5babeb967db23faac5c513fc29453fa8ec8c486704950da3d7461dd640
    ? ./utils/package.dhall
, xdg-open =
      ./xdg-open/package.dhall sha256:141cea3791cf64099e76ae3245f60c248c386fce353c68d410d297af2d4191b8
    ? ./xdg-open/package.dhall
, yarn =
      ./yarn/package.dhall sha256:d021db511fb8008e13ee8bcbfb2760acfd1464d0ecff5313e733b056924d53a5
    ? ./yarn/package.dhall
, youtube-dl =
      ./youtube-dl/package.dhall sha256:6500e88b107a9b7676cb3c3c46a1a237c1e13b7f529bab854502299bf4104240
    ? ./youtube-dl/package.dhall
}
