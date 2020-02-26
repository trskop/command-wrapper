-- vim: filetype=dhall

{ Options =
      ./Options/package.dhall sha256:c1efa59447b38551133781d90107aa3bed9e1b48abc4657faf8e9cac9e9ae07e
    ? ./Options/package.dhall
, command =
      ./command sha256:738ef1fdb2c436927461c1e115e90ed0761f305e145c61e33e8beb2569fa851c
    ? ./command
, completion =
      ./completion sha256:1821ae0233c49329b0e3d75d83ecf832708a30b7f3ac56152bf46f9d6e6382f9
    ? ./completion
}
