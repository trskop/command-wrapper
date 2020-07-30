-- vim: filetype=dhall

{ Options =
      ./Options/package.dhall sha256:5ef1501d56d05f15b5298302ae055f7b7edf0020831ad2edb375c28f7e12332e
    ? ./Options/package.dhall
, Variable =
      ./Variable/package.dhall sha256:61c8e76b8bef4eaeb117df69184badbd12ed5a5eccc59c51123942dde3079338
    ? ./Variable/package.dhall
, command =
      ./command.dhall sha256:9c9d36975a1d4e133cbbeb7ebd3c20c29a619c5c3cc57d33a8ff7a884743e48a
    ? ./command.dhall
, completion =
      ./completion.dhall sha256:a4e110141992c0386d1d994d33da20c5eb70498c620571b874e35c1e9d41b276
    ? ./completion.dhall
, completion-script =
      ./completion-script.dhall sha256:0308f23f3bf4dbc8c92a2340710357f790312dc8034f2e6b73f7a9d43b81f91e
    ? ./completion-script.dhall
}
