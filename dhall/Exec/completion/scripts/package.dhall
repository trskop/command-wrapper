-- vim: filetype=dhall

{ bazel =
      ./bazel.bash sha256:732b498c252fe00f55da10780ca9b31ae0b56237741c97e778313556ec26e027 as Text
    ? ./bazel.bash as Text
, direnv =
      ./direnv.bash sha256:a0b1bb86542e3a365eab2ed0aa074e2535cbd98472df962af0db40f034f16c0d as Text
    ? ./direnv.bash as Text
, docker-compose =
      ./docker-compose.bash sha256:79a6f96409191b948959c757703b94c3ebbb7cf5c86a013c769e23fc84bc7fd7 as Text
    ? ./docker-compose.bash as Text
, go-jira =
      ./go-jira.bash sha256:a2bed0ae3a53770a6d69132921baf9610199303beeefe7c84db0d45666d4ea2b as Text
    ? ./go-jira.bash as Text
, nix =
      ./nix.bash.dhall sha256:ee3faf5bb825d273b8bc87d7028fb8a489bf12de57af2df864780ffcb027c7b0
    ? ./nix.bash.dhall
, yarn =
      ./yarn.bash.dhall sha256:db0138e0317322afb5e9a9e71b29754cd1bb05641440cce1a33c987b2d679fee
    ? ./yarn.bash.dhall
}
