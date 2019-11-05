-- vim: filetype=dhall

{ GlobalOptions =
      ./GlobalOptions/package.dhall sha256:6441c690d791c994e423ef3c114beee71789ab05b95676db904dfb9dbf658e83
    ? ./GlobalOptions/package.dhall
, ExecOptions =
      ./ExecOptions/package.dhall sha256:f6d35c0450b0cad33b3e7a86009c45e4cd40948d861bcad5718a3da91ba97833
    ? ./ExecOptions/package.dhall
, RunOptions =
      ./RunOptions/package.dhall sha256:93d55484d93a91bfff9e23724bc6cf9dc299af1b50bf6f5eb759e4ef33cb6396
    ? ./RunOptions/package.dhall
, Environment =
      ./Environment/package.dhall sha256:a514274b0ed1d4135260eebbcc9ff6bae8078c0492f1cd54e4afab78cd8ef634
    ? ./Environment/package.dhall
, prune =
      ./prune sha256:2230f97860f1868581aebdffb1f69bfc8eab0aac9bdf57b21e938937d6a85f23
    ? ./prune
, exec =
      ./exec sha256:0a7e8673c232b8f769b127111da64c7cb1c7694c2248485bea0c658eb0e962fd
    ? ./exec
, run =
      ./run sha256:da02a2440e0e3733ec8d287336c4de2c4e85eb718eabdf0773eeddc4b6fe9ece
    ? ./run
}
