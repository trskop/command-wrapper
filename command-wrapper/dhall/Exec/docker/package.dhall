-- vim: filetype=dhall

{ GlobalOptions =
      ./GlobalOptions/package.dhall sha256:5184ed97270eb47233b1659e4c39012abb80151fdad7c42fd0dc1efd21aeb8d2
    ? ./GlobalOptions/package.dhall
, ExecOptions =
      ./ExecOptions/package.dhall sha256:57d80545f5e3af80afa17c2325e7c0f3297d8cf0168fa7577e081b86b5621701
    ? ./ExecOptions/package.dhall
, RunOptions =
      ./RunOptions/package.dhall sha256:0145c7c3ba8b966fec2e0644c12b8b33ac8a008dc084df42bce4b5d077597734
    ? ./RunOptions/package.dhall
, Environment =
      ./Environment/package.dhall sha256:a514274b0ed1d4135260eebbcc9ff6bae8078c0492f1cd54e4afab78cd8ef634
    ? ./Environment/package.dhall
, prune =
      ./prune sha256:71a0016257ab2a81b1c90b07dba7a3f885766984b28709492cbddaeb6160d1cc
    ? ./prune
, exec =
      ./exec sha256:cf8021fbd48361a89256e3c1cc468239f2b1f2a071cf914ca8e085df0bdc412b
    ? ./exec
, run =
      ./run sha256:064c11732d416dae04557fd465f90bb7fbcf4843af3c73582ba604e4f83d284b
    ? ./run
}
