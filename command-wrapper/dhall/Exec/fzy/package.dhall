-- vim: filetype=dhall

{ command =
      ./command sha256:92e23a1c41c41e50a3469ed94a10579b73c8835ec93c4380d1c3e839c8ff109e
    ? ./command
, Options =
      ./Options/package.dhall sha256:623fee89f9b4ffa70c77db65ca18768bedb26cdca8833297a787e27342de9abb
    ? ./Options/package.dhall
}
