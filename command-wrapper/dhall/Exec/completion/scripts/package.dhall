-- vim: filetype=dhall

{ bazel =
      ./bazel.bash sha256:732b498c252fe00f55da10780ca9b31ae0b56237741c97e778313556ec26e027 as Text
    ? ./bazel.bash as Text
, direnv =
      ./direnv.bash sha256:806c6f8632ddddedfd3c93330f26f074078bc26c4ffd91a232e589385f198c51 as Text
    ? ./direnv.bash as Text
, docker-compose =
      ./docker-compose.bash sha256:79a6f96409191b948959c757703b94c3ebbb7cf5c86a013c769e23fc84bc7fd7 as Text
    ? ./docker-compose.bash as Text
, go-jira =
      ./go-jira.bash sha256:a2bed0ae3a53770a6d69132921baf9610199303beeefe7c84db0d45666d4ea2b as Text
    ? ./go-jira.bash as Text
, jq =
      ./jq.bash.dhall sha256:0308f23f3bf4dbc8c92a2340710357f790312dc8034f2e6b73f7a9d43b81f91e
    ? ./jq.bash.dhall
, nix =
      ./nix.bash.dhall sha256:4a79f8b86f6ace664d44b60d10e6b1494a64dd339e071979fd3bcbf4d73d9e1c
    ? ./nix.bash.dhall
, yarn =
      ./yarn.bash.dhall sha256:65bc18ba5131cb50564c7fe48231b28d0c1fd4680548248cd078ab1530e64ab3
    ? ./yarn.bash.dhall
, youtube-dl =
      ./youtube-dl.bash.dhall sha256:4944c2e5cf852aa4969dae360495e92a53a5530def414b0090d0a691dc3351e6
    ? ./youtube-dl.bash.dhall
}