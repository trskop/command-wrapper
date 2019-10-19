-- vim: filetype=dhall

{ bazel =
      ./bazel/package.dhall sha256:1128d37404f94a83a1d084a38605eb0e7d45856fbae20ee49ccadaad4f2a0b6d
    ? ./bazel/package.dhall
, buildifier =
      ./buildifier/package.dhall sha256:2db5e3723ff4a17092c826b9d9200eab106d720dd940c7b4024d5a847e68f404
    ? ./buildifier/package.dhall
, completion =
      ./completion/package.dhall sha256:e5a4d2f2a69a37cc1ec26cfd5f8f42cafd9c012aab8a42350e20f214ae876632
    ? ./completion/package.dhall
, direnv =
      ./direnv/package.dhall sha256:5470faa0712aa2689e8390bb51426d4404ff321904f8f453ff98fb88faf42ec9
    ? ./direnv/package.dhall
, docker =
      ./docker/package.dhall sha256:b45a336cdda1a1c293273e607199982214b02c53d1f8f6a6895ad0b02d4721e7
    ? ./docker/package.dhall
, docker-compose =
      ./docker-compose/package.dhall sha256:1bb4247d70f469ad0c80b4afdf1b37b254fe6be672beee356c658f117234a5dd
    ? ./docker-compose/package.dhall
, firefox =
      ./firefox/package.dhall sha256:7296badbac888c2c8a78bf5590b44231fbca7cf88d368574bace43134d0d5b81
    ? ./firefox/package.dhall
, go-jira =
      ./go-jira/package.dhall sha256:399134ddae00f4c4617bba93550d6b12b56a18deb9bb548835f724b15a222093
    ? ./go-jira/package.dhall
, nix =
      ./nix/package.dhall sha256:bef79f326580afeab086a18701d379cab8b3b3d1c8e9f0cbfe52e8ee3b2cf8d9
    ? ./nix/package.dhall
, pg_dump =
      ./pg_dump/package.dhall sha256:0c0e9c664275c44fe289adf4972a0c8e16afc8d72238ab3c7eb449fa4a479628
    ? ./pg_dump/package.dhall
, psql =
      ./psql/package.dhall sha256:be18fe8eec49904ca79689ecbda31ec29b7fd26e12fc562a25ab9f5550b4fa76
    ? ./psql/package.dhall
, run-mailcap =
      ./run-mailcap/package.dhall sha256:7922c36cd92606a72b5ac90addc6b81ade313942b8142c5f57e5fcc79a640829
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
      ./yarn/package.dhall sha256:d345659d2980ed9e8fe8af6bd104a35d6d9a828b476b32a97e7cdcce1bb64aff
    ? ./yarn/package.dhall
}
