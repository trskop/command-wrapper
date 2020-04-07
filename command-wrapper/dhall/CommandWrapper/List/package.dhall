-- vim: filetype=dhall
--
-- TODO: Remove `head-and-tail` in favour of `headAndTail`.

{ head-and-tail =
      ./head-and-tail sha256:9e5de1b61d63f7caf1b197df7eb0ca19f682e6c05b4e1f557cd5a88355b31b08
    ? ./head-and-tail
, headAndTail =
      ./head-and-tail sha256:9e5de1b61d63f7caf1b197df7eb0ca19f682e6c05b4e1f557cd5a88355b31b08
    ? ./head-and-tail
, index =
      ./index sha256:275705f96ae6d2a2e2194489a9a3c9798fa9e7d6f7ff6847077d46efa3519e15
    ? ./index
}
