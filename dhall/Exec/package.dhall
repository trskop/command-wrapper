-- vim: filetype=dhall
--
-- Library of utilities for Command Wrapper's `exec` subcommand.

{ bazel =
      ./bazel/package.dhall sha256:ffc84b57c7927af361d65b9b49566550a0a2ef3b93115f7c3d16d7edfa50aedb
    ? ./bazel/package.dhall
, buildifier =
      ./buildifier/package.dhall sha256:2db5e3723ff4a17092c826b9d9200eab106d720dd940c7b4024d5a847e68f404
    ? ./buildifier/package.dhall
, completion =
      ./completion/package.dhall sha256:8f6de679496be3752f8fedafd1c2d656ada9a542903d267baeb9f832297e800c
    ? ./completion/package.dhall
, direnv =
      ./direnv/package.dhall sha256:1ba186cf3aed6b91df6b68f2188a4603a1e981fcb144bd5b4155c28ee872ff6b
    ? ./direnv/package.dhall
, docker =
      ./docker/package.dhall sha256:bd928e08c02d378dd396bf12b07fbbd56e24ff5339697b9d3176b7bb3b106fe1
    ? ./docker/package.dhall
, docker-compose =
      ./docker-compose/package.dhall sha256:8dd20e7da68b09a1782ca9ec35ee7e99847d8ff5e77e1ba01efaa1f9bc04bcd6
    ? ./docker-compose/package.dhall
, firefox =
      ./firefox/package.dhall sha256:7296badbac888c2c8a78bf5590b44231fbca7cf88d368574bace43134d0d5b81
    ? ./firefox/package.dhall
, go-jira =
      ./go-jira/package.dhall sha256:0d806fda968ea4a93aa8168a337bafd657e9190c20dbbfc31ee87155a939bf87
    ? ./go-jira/package.dhall
, jq =
      ./jq/package.dhall sha256:bb1084d4f76ffa6194f85d44e8849d79e5af355072ec8b2852c2ffac5fef1ac1
    ? ./jq/package.dhall
, nix =
      ./nix/package.dhall sha256:9467f2618cde29196c6df3b79f564617fd2a3c0d2584fa8189d0efff6f94e1d4
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
      ./yarn/package.dhall sha256:9a12479b096de216da11a699e26774a7546e09e8ec7bed343f6fc4bd06b3c019
    ? ./yarn/package.dhall
}
