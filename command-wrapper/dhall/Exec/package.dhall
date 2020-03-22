-- vim: filetype=dhall
--
-- Library of utilities for Command Wrapper's `exec` subcommand.

{ bazel =
      ./bazel/package.dhall sha256:913b8199d3582d6c470c89477047674f880803effc084912149810ea3e161b93
    ? ./bazel/package.dhall
, buildifier =
      ./buildifier/package.dhall sha256:2db5e3723ff4a17092c826b9d9200eab106d720dd940c7b4024d5a847e68f404
    ? ./buildifier/package.dhall
, command-wrapper =
      ./command-wrapper/package.dhall sha256:196834fd31413e78d5f8e28d6dfec488e74b8bf21f7e90c20d610bdca3c880bb
    ? ./command-wrapper/package.dhall
, completion =
      ./completion/package.dhall sha256:010acfececfce2a8f66530f258ced7a7ffd5769cc3e5d22b4e13f7e520eaeccf
    ? ./completion/package.dhall
, direnv =
      ./direnv/package.dhall sha256:b36088850be921f181acbc8c8bbd17f61a7d7d4da04da4826b053aed02e915ff
    ? ./direnv/package.dhall
, docker =
      ./docker/package.dhall sha256:13ee98abc91fa54c5b43d77d1d855c6fb4776cff0daaca961fb3de451bb95740
    ? ./docker/package.dhall
, docker-compose =
      ./docker-compose/package.dhall sha256:918bb2cff00d6819cce47fde848a14ff89db60a2b6945800657d7d3a57710369
    ? ./docker-compose/package.dhall
, firefox =
      ./firefox/package.dhall sha256:7f6e5fe7087c2dbc297349b4640c9a5a1e04ebe378254fedb254e9a7b2c94140
    ? ./firefox/package.dhall
, fzf =
      ./fzf/package.dhall sha256:2fc9c6ca3407568f592a0ffb8c82a57a458373f2fa1235d836ef68971fe525cb
    ? ./fzf/package.dhall
, fzy =
      ./fzy/package.dhall sha256:41e967ece673a9681c2ace686f8ff2579c532c9c11fb595294ee0e70ce2b77b2
    ? ./fzy/package.dhall
, go-jira =
      ./go-jira/package.dhall sha256:1fe5c6887dbc95f05fc41f4d9b46692194b7fee8897b00b7c719f9e2723ce42c
    ? ./go-jira/package.dhall
, jq =
      ./jq/package.dhall sha256:5126347c9c6e12a1f494af48598210ab8a75c3af1a586091239dcf49281a94ad
    ? ./jq/package.dhall
, nix =
      ./nix/package.dhall sha256:6da4a672f55b1ffc03e37d736a1bf88fcf1d9f8aa5f7a3392d0c89d81f350bba
    ? ./nix/package.dhall
, nvr =
      ./nvr/package.dhall sha256:f48a7ed1672b16c8b1442e536d348b22fe809648ba5cf6bf614fd686f3747d69
    ? ./nvr/package.dhall
, pg_dump =
      ./pg_dump/package.dhall sha256:0c0e9c664275c44fe289adf4972a0c8e16afc8d72238ab3c7eb449fa4a479628
    ? ./pg_dump/package.dhall
, psql =
      ./psql/package.dhall sha256:be18fe8eec49904ca79689ecbda31ec29b7fd26e12fc562a25ab9f5550b4fa76
    ? ./psql/package.dhall
, run-mailcap =
      ./run-mailcap/package.dhall sha256:86ae21c3007712c87f519297d036135c33c9bf23bd89008b8134ee1907940d88
    ? ./run-mailcap/package.dhall
, sk =
      ./sk/package.dhall sha256:8b783d59f89be1b54cc4756dab39157f64695ad2460751f34af8c5e7afe86d71
    ? ./sk/package.dhall
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
      ./utils/package.dhall sha256:02ade7ee07b046bc6366979d944e3b6f9fbf68536bc6abae6431769f2cb94932
    ? ./utils/package.dhall
, xdg-open =
      ./xdg-open/package.dhall sha256:141cea3791cf64099e76ae3245f60c248c386fce353c68d410d297af2d4191b8
    ? ./xdg-open/package.dhall
, yarn =
      ./yarn/package.dhall sha256:68fa18f6854ddfa586dfaae5c8fbff4e994027b594e9d3f29389e47d1f63051e
    ? ./yarn/package.dhall
, youtube-dl =
      ./youtube-dl/package.dhall sha256:56270fcb6737f1c8a506c00c246ea61cf29ec118e5550095f37a18b95337938f
    ? ./youtube-dl/package.dhall
}
