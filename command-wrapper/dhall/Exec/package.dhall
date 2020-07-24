-- vim: filetype=dhall
--
-- Library of utilities for Command Wrapper's `exec` subcommand.

{ bazel =
      ./bazel/package.dhall sha256:803e8d9fd4ba30d04f31820a166b6b1851515a20ac4325b7d80b3f75a587e2e3
    ? ./bazel/package.dhall
, buildifier =
      ./buildifier/package.dhall sha256:2db5e3723ff4a17092c826b9d9200eab106d720dd940c7b4024d5a847e68f404
    ? ./buildifier/package.dhall
, command-wrapper =
      ./command-wrapper/package.dhall sha256:cbeba6e699f4571419b7f6679960ecede274036852fef02b720423990fbbffd1
    ? ./command-wrapper/package.dhall
, completion =
      ./completion/package.dhall sha256:ed0b26d74ce06f5fb0c52520c472b9a2447bbf3ae7c84dfbb5c250cc83c3779d
    ? ./completion/package.dhall
, direnv =
      ./direnv/package.dhall sha256:35316c76d4b8476a0f7b2bb4f6e246af54bf9d9ad4c0cc268330737bcdfd7104
    ? ./direnv/package.dhall
, docker =
      ./docker/package.dhall sha256:c24d100c608543261d14fe487501691086fbba63e4465c896c78e1d592503c49
    ? ./docker/package.dhall
, docker-compose =
      ./docker-compose/package.dhall sha256:de65bcfc6b794f3f15084ac10be3ce2fff9f9ecd37c42d0bed6728b79df690dd
    ? ./docker-compose/package.dhall
, env =
      ./env/package.dhall sha256:64a11ad3e1dbb71ebef8b4da50099048bb28d5c54f8467127c42b1035625ce22
    ? ./env/package.dhall
, firefox =
      ./firefox/package.dhall sha256:a3ce24f911e9e10fd6951be4601db8143a2feb7abfc2256aea23eae06fb50224
    ? ./firefox/package.dhall
, fzf =
      ./fzf/package.dhall sha256:99b3850be706e2642c9060dbff336b3701a3a5f5d2f8dc5ff6d147d824c673d1
    ? ./fzf/package.dhall
, fzy =
      ./fzy/package.dhall sha256:a8af06d08a2b0175b8c4b94b4feb82ce17c3e5aaac6778e14b1265cea356c380
    ? ./fzy/package.dhall
, go-jira =
      ./go-jira/package.dhall sha256:5a78db0adee1a5660d93999f4c99f7d1fb8e1d0e238e6a55ece90de6da7a000a
    ? ./go-jira/package.dhall
, jq =
      ./jq/package.dhall sha256:36b918283caa0db9bf4ae8415cd089046040fd3a0d487b67c3ac597f25fea0d7
    ? ./jq/package.dhall
, nix =
      ./nix/package.dhall sha256:5639ea202b2f759cbdc0d9cb6ef30187436d1f2720ad4815253f4a5111ff92c5
    ? ./nix/package.dhall
, nvr =
      ./nvr/package.dhall sha256:930aec8341a97865603cb0b8e7dd000d1193cb301b3f266033c0452cb740a9e8
    ? ./nvr/package.dhall
, pg_dump =
      ./pg_dump/package.dhall sha256:0c0e9c664275c44fe289adf4972a0c8e16afc8d72238ab3c7eb449fa4a479628
    ? ./pg_dump/package.dhall
, psql =
      ./psql/package.dhall sha256:a9a1b39fd151ba677845d4a66800620c9444f34a9c29182c7f20ffd82c2ade2d
    ? ./psql/package.dhall
, run-mailcap =
      ./run-mailcap/package.dhall sha256:86ae21c3007712c87f519297d036135c33c9bf23bd89008b8134ee1907940d88
    ? ./run-mailcap/package.dhall
, shake =
      ./shake/package.dhall sha256:b1e5996ed63293292e54b01163ba0d251d5baa75cb9dfc47085f7402e7770bed
    ? ./shake/package.dhall
, sk =
      ./sk/package.dhall sha256:ff1d4f30e9bb06ef48b73a6838ab467d589734899e9f1dfb24cc6d72bc02b9e6
    ? ./sk/package.dhall
, ssh =
      ./ssh/package.dhall sha256:4cbcd83c578ec8c931abc795792789858282ad0c4d3d6ddb10c0cfde87de4e7e
    ? ./ssh/package.dhall
, stack =
      ./stack/package.dhall sha256:7566be806f87b251a526417ac9267c7525499d44808ae624db6e9e5ab6729eeb
    ? ./stack/package.dhall
, tmux =
      ./tmux/package.dhall sha256:f3c04c719469147839d3fc8a383d6ccc09d25995ea483d820291899770974a76
    ? ./tmux/package.dhall
, utils =
      ./utils/package.dhall sha256:6fec064c3da0821bc089c84d9b6bad79514ee19802b2493b0ec00d4e7a9c192b
    ? ./utils/package.dhall
, xdg-open =
      ./xdg-open/package.dhall sha256:fb51326ee0c34d4be154e0dacaefe19f23d29896fca8e32c9bfb3a41fcd76e42
    ? ./xdg-open/package.dhall
, yarn =
      ./yarn/package.dhall sha256:efaed34b7c5e6525181ad9598cab32386f033d0e0e5e00a70953a04590715927
    ? ./yarn/package.dhall
, youtube-dl =
      ./youtube-dl/package.dhall sha256:6f2cb492a2f234014fc9a034e774d04c049b6c61af56c6d2ecb85c6492c1dde2
    ? ./youtube-dl/package.dhall
}
