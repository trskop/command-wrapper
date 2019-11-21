-- vim: filetype=dhall
--
-- Library of utilities for Command Wrapper's `exec` subcommand.

{ bazel =
      ./bazel/package.dhall sha256:19ce9c83d483376398e6d5e68af3f96d4d29b2fff5829758b373151d5a6ced6d
    ? ./bazel/package.dhall
, buildifier =
      ./buildifier/package.dhall sha256:2db5e3723ff4a17092c826b9d9200eab106d720dd940c7b4024d5a847e68f404
    ? ./buildifier/package.dhall
, completion =
      ./completion/package.dhall sha256:aeb633bf3ba8bdf289c5a678dd0370d8c226f1a88b8d466d77363befe59a42e3
    ? ./completion/package.dhall
, direnv =
      ./direnv/package.dhall sha256:9de4d52ec764c84bde6e8ba4397cd58e1bcb8d536bac1814c3d3db5cec70b5ad
    ? ./direnv/package.dhall
, docker =
      ./docker/package.dhall sha256:13ee98abc91fa54c5b43d77d1d855c6fb4776cff0daaca961fb3de451bb95740
    ? ./docker/package.dhall
, docker-compose =
      ./docker-compose/package.dhall sha256:0d0911e8d551c87543c4581777efd9d0afe3e00dddfd13c40ad7a8b7f6e717bc
    ? ./docker-compose/package.dhall
, firefox =
      ./firefox/package.dhall sha256:7f6e5fe7087c2dbc297349b4640c9a5a1e04ebe378254fedb254e9a7b2c94140
    ? ./firefox/package.dhall
, go-jira =
      ./go-jira/package.dhall sha256:f9877f2a5c47de68746fa22efa12987d05cec20058af9010a1ec10188444afd0
    ? ./go-jira/package.dhall
, jq =
      ./jq/package.dhall sha256:ec516658452dbbafb09f47f266a4267ce5d53a930156bde18899b14eaacb7971
    ? ./jq/package.dhall
, nix =
      ./nix/package.dhall sha256:0c06472cb55162ef1b987b1b06098a0aa20f7295e37d471719169fd9ce60d505
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
      ./yarn/package.dhall sha256:93d35bc986bad6fbeaad50b0dd767d2db685c71c647fc9345943d4affec52b12
    ? ./yarn/package.dhall
, youtube-dl =
      ./youtube-dl/package.dhall sha256:218ec15b45fbe6848a8eb5ca916a773b9e04ce49232bf5f148e25abd0dde1d24
    ? ./youtube-dl/package.dhall
}
