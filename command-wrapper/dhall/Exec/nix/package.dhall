-- vim: filetype=dhall

{ Command =
      ./Command/package.dhall sha256:59105ba8b28630c540c527be9ee60b8f5408a806c4b0689163280dc7a00f916d
    ? ./Command/package.dhall
, command =
      ./command sha256:2cbde157ed5b0a2f5f4c9dbdcccacda52e543e5ca50fd9642094a11894e22e11
    ? ./command
, completion =
      ./completion sha256:00fdde2edd648423b17322313697f69b3f148559048237acedc7d75bd29b3a81
    ? ./completion
, completion-script =
      ./completion-script sha256:4a79f8b86f6ace664d44b60d10e6b1494a64dd339e071979fd3bcbf4d73d9e1c
    ? ./completion-script
}
