-- vim: filetype=dhall

{ headAndTail =
      ./headAndTail sha256:06d3b92abb0790387092bf6db0ff9fc20299a898bb9e9a8e34373ad92b5ed86d
    ? ./headAndTail
, index =
      ./index sha256:ef2a9b5ac2dcebc7bbde771c911903af6a41099ea8ffdb2dabe8538277afec4d
    ? ./index
}
