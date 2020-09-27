-- vim: filetype=dhall
--
-- Library of utilities for Command Wrapper's `exec` subcommand.

{ bazel =
      ./bazel/package.dhall sha256:4addce3a488a5bf3bd134416dd014ac5982ffa55e3c78f646fc5f81a942335cc
    ? ./bazel/package.dhall
, buildifier =
      ./buildifier/package.dhall sha256:2db5e3723ff4a17092c826b9d9200eab106d720dd940c7b4024d5a847e68f404
    ? ./buildifier/package.dhall
, command-wrapper =
      ./command-wrapper/package.dhall sha256:a42429cb5f2ba9219d55d601c71f9307581a221e8e4fe096e2bbe99399ac3b2e
    ? ./command-wrapper/package.dhall
, CommonOptions =
      ./CommonOptions/package.dhall sha256:c998cb1060674fb2160f299f0a50bafd2d52052d451a04bee21d91a01731e03d
    ? ./CommonOptions/package.dhall
, completion =
      ./completion/package.dhall sha256:ed0b26d74ce06f5fb0c52520c472b9a2447bbf3ae7c84dfbb5c250cc83c3779d
    ? ./completion/package.dhall
, direnv =
      ./direnv/package.dhall sha256:8d6e9e3fe22e08c2a13b263bc9e8c3c0bda041ce9cfd7e565b23368833377168
    ? ./direnv/package.dhall
, docker =
      ./docker/package.dhall sha256:be338ccca00b1ca024bcf7fc92f55a2b3b976bf1334361754a4cbb35b83e51f2
    ? ./docker/package.dhall
, docker-compose =
      ./docker-compose/package.dhall sha256:8eabc80d45193802ca2047df273b151ebb4175259887de8dc27d6e842f742387
    ? ./docker-compose/package.dhall
, env =
      ./env/package.dhall sha256:5b7cda7caab327f1fc582d5143d480702b52daae69bfbd82698a41891ee608fd
    ? ./env/package.dhall
, firefox =
      ./firefox/package.dhall sha256:8b8bfab1f13d232671a4cfed1ac602732b17eaf914e0a5d862cf29522902b101
    ? ./firefox/package.dhall
, fzf =
      ./fzf/package.dhall sha256:7991bf4f95ab1752d4e7a231b977dd155ff22de5ce45939fbfd347e582969f85
    ? ./fzf/package.dhall
, fzy =
      ./fzy/package.dhall sha256:31becbf874ea04bfe5024a4338414642a7c127aac06bcd31be186b1c1f6c16ef
    ? ./fzy/package.dhall
, go-jira =
      ./go-jira/package.dhall sha256:6ed0bf294ff1b541064d63d6ad95871994859135b4937127aca17af8c3fe55a1
    ? ./go-jira/package.dhall
, jq =
      ./jq/package.dhall sha256:a781577632677e4a167b18a2f7b433e85a3b7609da62cfa4e49c4a0d10b13327
    ? ./jq/package.dhall
, nix =
      ./nix/package.dhall sha256:3f897c747f9988853bb2d030c8e82bd7ebd9f09b4ce787bad77a8ee9aa9fcd36
    ? ./nix/package.dhall
, nvr =
      ./nvr/package.dhall sha256:6372c79b6ee979a3f977ceb9871f12741e49c7e4fef79c72587c5a7f75f8f8a3
    ? ./nvr/package.dhall
, pg_dump =
      ./pg_dump/package.dhall sha256:0c0e9c664275c44fe289adf4972a0c8e16afc8d72238ab3c7eb449fa4a479628
    ? ./pg_dump/package.dhall
, ProxySettings =
      ./ProxySettings/package.dhall sha256:4371e8f4fa794ae56150a1213ed31f11abb57d217506963d9ece4ad8cac319f4
    ? ./ProxySettings/package.dhall
, psql =
      ./psql/package.dhall sha256:d5cc5d88de983ea1d3217bbf7a96f2b53d29260bf7221231bfa0e5958475a141
    ? ./psql/package.dhall
, run-mailcap =
      ./run-mailcap/package.dhall sha256:4d699c79831b485bee35be93206a19f3db0a853dc2bbfa615afd65963e6c28d4
    ? ./run-mailcap/package.dhall
, shake =
      ./shake/package.dhall sha256:50bf90d98c7a341307eed16e60200dbff05eebd33e266f41255574193b517959
    ? ./shake/package.dhall
, sk =
      ./sk/package.dhall sha256:08118a84422709ab97c770ef0a7c8317e31cf0dff64bc8b46e44c83558ec5010
    ? ./sk/package.dhall
, ssh =
      ./ssh/package.dhall sha256:69a3712f8b813d55a0fe89d97ee938c35c57acdb8c5ae50fa8d9bc6582038fa8
    ? ./ssh/package.dhall
, stack =
      ./stack/package.dhall sha256:7566be806f87b251a526417ac9267c7525499d44808ae624db6e9e5ab6729eeb
    ? ./stack/package.dhall
, tmux =
      ./tmux/package.dhall sha256:39ca6eb7e94289d4796bab4a6b58ef15f8d81f7c3a38cebd1dd7b7c5c27b7cee
    ? ./tmux/package.dhall
, utils =
      ./utils/package.dhall sha256:6fec064c3da0821bc089c84d9b6bad79514ee19802b2493b0ec00d4e7a9c192b
    ? ./utils/package.dhall
, xdg-open =
      ./xdg-open/package.dhall sha256:fb51326ee0c34d4be154e0dacaefe19f23d29896fca8e32c9bfb3a41fcd76e42
    ? ./xdg-open/package.dhall
, yarn =
      ./yarn/package.dhall sha256:3b553059f72c17364a8d3caa0db56b055d0935820f2ac282616bdfacdfdbcfc8
    ? ./yarn/package.dhall
, youtube-dl =
      ./youtube-dl/package.dhall sha256:0afc6dd7d58b4c339a2fef0cf1e11735cab9a98219c46a58a6442a54b3de610e
    ? ./youtube-dl/package.dhall
}
