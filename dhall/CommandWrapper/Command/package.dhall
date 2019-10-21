-- vim: filetype=dhall
--
-- Smart constructor `simple` is deprecated and should be removed at some
-- point.  Use `::` operator instead.

{ Type =
      ./Type sha256:81b4cf2d0952d4a805a40ef1ccc82b28a1d339586f2d44c565ee263457696219
    ? ./Type
, default =
      ./default sha256:45177da2a98a4e2d6b2e879caef7c2d7b6ea804feb18ed69577293ee8c7b283c
    ? ./default
, emptyArguments =
      ./emptyArguments sha256:6da0c98142d1168ac9d6080b7853d2b59b97d42079e1b2f121daf449b3b2e449
    ? ./emptyArguments
, emptyEnvironment =
      ./emptyEnvironment sha256:cd98ff0deea70057d59baf060c91fba887fc149198b7998ba6581a08c4793a6e
    ? ./emptyEnvironment
, simple =
      ./simple sha256:3e92f6633519a6760c4f815914d68f706c4140ab3d5424619a34a39573de9c42
    ? ./simple
, withExtraArguments =
      ./withExtraArguments sha256:df4bdfa77e9c3034e3a7749b5a7ef461f9b64e51d897d733f71a9cf7a889e971
    ? ./withExtraArguments
}
