-- vim: filetype=dhall

{ Command =
      ./Command/Type sha256:dc2b111daadb4265396dc562b757f208189242a884e704a4298686ce8984af27
    ? ./Command/Type
, command =
      ./command sha256:2cbde157ed5b0a2f5f4c9dbdcccacda52e543e5ca50fd9642094a11894e22e11
    ? ./command
, completion =
      ./completion sha256:8564400b0feaeda7590cd3eb0c6f1bd2830bdde0c8c259b1207f5e312718165f
    ? ./completion
, completion-script =
      ./completion-script sha256:4a79f8b86f6ace664d44b60d10e6b1494a64dd339e071979fd3bcbf4d73d9e1c
    ? ./completion-script
, Command/show =
      ./Command/show sha256:802eda931b470db5c62b8c19260b9da1c87f5ea05ef5317ee241d87d9bc05961
    ? ./Command/show
}
