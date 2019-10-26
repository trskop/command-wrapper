-- vim: filetype=dhall

{ bazel =
      ./bazel/package.dhall sha256:f7500f8d2730acd5a16cbbab756276cbac4799fd35b6639fb0860dba97ac3da7
    ? ./bazel/package.dhall
, buildifier =
      ./buildifier/package.dhall sha256:2db5e3723ff4a17092c826b9d9200eab106d720dd940c7b4024d5a847e68f404
    ? ./buildifier/package.dhall
, completion =
      ./completion/package.dhall sha256:7f320680b49adfdc8a3e0406675dc92e9aea796a37558fe720b218a9ccf815a7
    ? ./completion/package.dhall
, direnv =
      ./direnv/package.dhall sha256:3f2befdc67cb37b9538f445baac73cfd7dfa5c18d2b350adcf13ab3f8047ce61
    ? ./direnv/package.dhall
, docker =
      ./docker/package.dhall sha256:bd928e08c02d378dd396bf12b07fbbd56e24ff5339697b9d3176b7bb3b106fe1
    ? ./docker/package.dhall
, docker-compose =
      ./docker-compose/package.dhall sha256:42f113616b271e40fc6fcb3716616b1d697d9126071794291bd0b4a471c56cbc
    ? ./docker-compose/package.dhall
, firefox =
      ./firefox/package.dhall sha256:7296badbac888c2c8a78bf5590b44231fbca7cf88d368574bace43134d0d5b81
    ? ./firefox/package.dhall
, go-jira =
      ./go-jira/package.dhall sha256:36854ec988f6ed9021d5c863e1373004cc108f17177978ca4918c0d846924734
    ? ./go-jira/package.dhall
, nix =
      ./nix/package.dhall sha256:89d94de59db98b217e06a77956750dfd2fe0767f3123e17245b1fbae8a1b8879
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
      ./utils/package.dhall sha256:fc1713c85fecdbaf8ae5cc83d5ca51ce61b700007429fad16fb8b0b82b383952
    ? ./utils/package.dhall
, xdg-open =
      ./xdg-open/package.dhall sha256:141cea3791cf64099e76ae3245f60c248c386fce353c68d410d297af2d4191b8
    ? ./xdg-open/package.dhall
, yarn =
      ./yarn/package.dhall sha256:d4e35b5a7574d2c33125d94bbf4af50bbdccf100eba34992c067662ddf3c2341
    ? ./yarn/package.dhall
}
