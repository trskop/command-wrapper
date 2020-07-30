-- vim: filetype=dhall

{ command =
      ./command.dhall sha256:b8ce7a7f4fa99d7177dd205c1e86e3f61210603cfa19d4013054ee3d8fad523c
    ? ./command.dhall
, completion-script =
      ./completion-script.dhall sha256:80d3eecf7ea99ec629c1acf0d852a50a80394d3ed18801f3af83a1190d70cd20
    ? ./completion-script.dhall
, completion =
      ./completion.dhall sha256:592a3515000f91da95888f7e8c4b1424ed2142d9f2e47119a01073df5ecd24c9
    ? ./completion.dhall
}
