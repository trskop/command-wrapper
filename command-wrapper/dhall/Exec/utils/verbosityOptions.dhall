-- vim: filetype=dhall
--
-- Convert `Verbosity` value into command line options.

let Verbosity =
      { Type =
            ../../CommandWrapper/Verbosity/Type sha256:f009a39a49b1ee65651e9510778e7d72ff96820f4702a955e8f47682d72995c6
          ? ../../CommandWrapper/Verbosity/Type
      , fold =
            ../../CommandWrapper/Verbosity/fold sha256:4dac2c264a2531d569ad0e5f712a1cd2d17b51ecdc502cc72f19937bf4733b1e
          ? ../../CommandWrapper/Verbosity/fold
      }

let verbosityOptions
    : { Silent : List Text
      , Normal : List Text
      , Verbose : List Text
      , Annoying : List Text
      } →
      Verbosity.Type →
        List Text
    = Verbosity.fold (List Text)

let example =
        assert
      :   verbosityOptions
            { Silent = [ "--quiet" ]
            , Normal = [] : List Text
            , Verbose = [ "--verbose" ]
            , Annoying = [ "--debug" ]
            }
            Verbosity.Type.Verbose
        ≡ [ "--verbose" ]

in  verbosityOptions
