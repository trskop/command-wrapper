-- vim: filetype=dhall

{ Type =
      ./Type sha256:cdde9b8672815af3ab1345e0c8fe95fa945cf2f62be00da964136770a4e44933
    ? ./Type
, default =
      ./default sha256:56aa1a9952e189ceb0a3371ffa5c58ac97191f7a183f7f1535e3c0f5ae710868
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
