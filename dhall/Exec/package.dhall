-- vim: filetype=dhall
--
-- Library of utilities for Command Wrapper's `exec` subcommand.

{ bazel =
      ./bazel/package.dhall sha256:7892fbade0c80c1f6648e110bca90321430ab0e33a5bbdbed57399bc719ddb54
    ? ./bazel/package.dhall
, buildifier =
      ./buildifier/package.dhall sha256:2db5e3723ff4a17092c826b9d9200eab106d720dd940c7b4024d5a847e68f404
    ? ./buildifier/package.dhall
, completion =
      ./completion/package.dhall sha256:8f6de679496be3752f8fedafd1c2d656ada9a542903d267baeb9f832297e800c
    ? ./completion/package.dhall
, direnv =
      ./direnv/package.dhall sha256:9c6faad905a6300956f7338050facb3021b6731e6ad294c4cff41fa59933a0cf
    ? ./direnv/package.dhall
, docker =
      ./docker/package.dhall sha256:bd928e08c02d378dd396bf12b07fbbd56e24ff5339697b9d3176b7bb3b106fe1
    ? ./docker/package.dhall
, docker-compose =
      ./docker-compose/package.dhall sha256:17eabab1b4d0e77cdef949537754d2ed0a1992629869d108790f7a88882132ca
    ? ./docker-compose/package.dhall
, firefox =
      ./firefox/package.dhall sha256:7296badbac888c2c8a78bf5590b44231fbca7cf88d368574bace43134d0d5b81
    ? ./firefox/package.dhall
, go-jira =
      ./go-jira/package.dhall sha256:3818ebbfb5b97f722a58dc64da6fa350f75707a7461141b495f468c423bece8a
    ? ./go-jira/package.dhall
, jq =
      ./jq/package.dhall sha256:336e107c4102988e1204f5fcbdd291db27b3522f8660527c493d4fdf63cd7f45
    ? ./jq/package.dhall
, nix =
      ./nix/package.dhall sha256:aebd45163c35ba87817264a233f380c93722c08bc7b6c30f0d3a3bfe4b87ff5e
    ? ./nix/package.dhall
, pg_dump =
      ./pg_dump/package.dhall sha256:0c0e9c664275c44fe289adf4972a0c8e16afc8d72238ab3c7eb449fa4a479628
    ? ./pg_dump/package.dhall
, psql =
      ./psql/package.dhall sha256:be18fe8eec49904ca79689ecbda31ec29b7fd26e12fc562a25ab9f5550b4fa76
    ? ./psql/package.dhall
, run-mailcap =
      ./run-mailcap/package.dhall sha256:2edbd021d84677de13607ea6b0371ce0f852ef01ef8928891a64e5d39ca1d8a6
    ? ./run-mailcap/package.dhall
, ssh =
      ./ssh/package.dhall sha256:cd6938eef068ea19df133d5ce4ea2890c59f448dfc274744767aaff08d384f6c
    ? ./ssh/package.dhall
, stack =
      ./stack/package.dhall sha256:fe28540b75328d9ad29e78dbdba19655536a115d8a82683dadc230d4073b0bdd
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
      ./yarn/package.dhall sha256:cd6498365be7b846c018437685aac34a1f34fe4898627c5b84e6ceea5ee8fbe2
    ? ./yarn/package.dhall
}
