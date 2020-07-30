-- vim: filetype=dhall

{ command =
      ./command.dhall sha256:6bf57fb9c46ec8bf1e2ecf65aababbf86c9028f3f84ed3311d6ea2c2cf501977
    ? ./command.dhall
, completion =
      ./completion.dhall sha256:165dfcc32cc8e85c3687001663e21abd43b6717a55ae76cd33b4b0eeafac8a98
    ? ./completion.dhall
, completion-script =
      ./completion-script.dhall sha256:4944c2e5cf852aa4969dae360495e92a53a5530def414b0090d0a691dc3351e6
    ? ./completion-script.dhall
}
