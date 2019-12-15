-- vim: filetype=dhall

{ Type =
      ./Type sha256:dec266f74aa018b2f69fb0c5c97b1624788383759de620f5b5d117e3563b33d4
    ? ./Type
, default =
      ./default sha256:af07db775a06c1e83a47299f9d11d6c9ecd8c080755922bf5f2776f75d202957
    ? ./default
, emptyDirectories =
      ./emptyDirectories sha256:6da0c98142d1168ac9d6080b7853d2b59b97d42079e1b2f121daf449b3b2e449
    ? ./emptyDirectories
, menu-tool =
    { fzf =
          ./menu-tool/fzf sha256:f6f1587c1adf9736b8653a2c7e575452c217e1470fc5bac85b4136fb8138949d
        ? ./menu-tool/fzf
    , fzy =
          ./menu-tool/fzy sha256:20c4bb5892965553bb7cabe3de4e305552ebe8b397597fc863f1dd1ccf6b95e7
        ? ./menu-tool/fzy
    }
, systemShell =
      ./systemShell sha256:12acd324db4f2ef5f9a79ae53f8e325eea080ded9befc5f73fe2235a725e7c92
    ? ./systemShell
}
