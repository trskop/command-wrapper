-- vim: filetype=dhall

let Options =
      { Type =
            ./Type.dhall sha256:7a7eb5cc89d0f8434982f3c8c6e95026eab92e5b13e8223b8e2ef0066ef5cd3d
          ? ./Type.dhall
      }

let ShakeVerbosity =
        ../ShakeVerbosity/Type.dhall sha256:85aa28a8ed92e7924805f4501411c185014af50a9f3227b27fdaa17e495a68af
      ? ../ShakeVerbosity/Type.dhall

let CompactOutput =
        ../CompactOutput/Type.dhall sha256:3f7e82ca4628384d6df93a0c6b8813d1d38a438b096695cb219c24ffae9cc87b
      ? ../CompactOutput/Type.dhall

let Environment =
      { empty =
            ../../../CommandWrapper/Environment/empty sha256:cd98ff0deea70057d59baf060c91fba887fc149198b7998ba6581a08c4793a6e
          ? ../../../CommandWrapper/Environment/empty
      }

let default =
      { noBuild = False
      , directory = None Text
      , colour = False
      , compactOutput = None CompactOutput
      , jobs = None Natural
      , keepGoing = None Bool
      , skipCommands = False
      , verbosity = None ShakeVerbosity
      , printDirectory = None Bool
      , noTime = False
      , timings = False
      , environment = Environment.empty
      }

let consistency = assert : (Options ∧ { default })::{=} ≡ default

in  default
