-- vim: filetype=dhall

{ Type =
      ./Type.dhall sha256:dbef3a3dd77c5377d0f571fde20161110ea5f99383ee7ac3c97cdee6b72cab67
    ? ./Type.dhall
, default =
      ./default.dhall sha256:a6920a5071e8cd8468202daf08c7ea5727803a98dc83311a27265cfb7a351041
    ? ./default.dhall
, toArguments =
      ./toArguments.dhall sha256:81b5506dddf93456fef3efe9ba921abaa3f2e36476901a089542ae31cdd0befc
    ? ./toArguments.dhall
, toEnvironment =
      ./toEnvironment.dhall sha256:1e5716d2c376e0af207ad34169a21797aedda8dd1c7d56ae156e1dfa7e724ca1
    ? ./toEnvironment.dhall
}
