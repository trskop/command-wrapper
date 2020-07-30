-- vim: filetype=dhall

{ Options =
      ./Options/package.dhall sha256:e0bd7ba7835adf0dd851c5fddf4e86f9e39712a802f161f4d79ccf6f733bd227
    ? ./Options/package.dhall
, command =
      ./command.dhall sha256:eb99d7ed2460056cd487ac1445e33974bc9f1ae8f423a5c0a1522ea69940b3f9
    ? ./command.dhall
, completion =
      ./completion.dhall sha256:b7ff485d5e63f889f0f29469626948c65a4575563ec4c4ccdfc9fbe6b80d4dd1
    ? ./completion.dhall
, completion-script =
      ./completion-script.dhall sha256:a2bed0ae3a53770a6d69132921baf9610199303beeefe7c84db0d45666d4ea2b
    ? ./completion-script.dhall
}
