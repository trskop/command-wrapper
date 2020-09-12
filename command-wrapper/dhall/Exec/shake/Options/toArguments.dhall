-- vim: filetype=dhall

let Options =
      { Type =
            ./Type.dhall sha256:7a7eb5cc89d0f8434982f3c8c6e95026eab92e5b13e8223b8e2ef0066ef5cd3d
          ? ./Type.dhall
      , default =
            ./default.dhall sha256:179b7a2158c765d02808b502bdb6fc40c20879852e7965c8b0b8345ba9afe269
          ? ./default.dhall
      }

let ShakeVerbosity =
        ../ShakeVerbosity/package.dhall sha256:3d0bf0eede2a1cc535bb505f3980db0a6757565bf82891c161ce60cff11d4363
      ? ../ShakeVerbosity/package.dhall

let CompactOutput =
        ../CompactOutput/package.dhall sha256:b461366e726abb1c388290a6110f2b790f06590e11520069f13d2bcadc585e9b
      ? ../CompactOutput/package.dhall

let optionalFlags =
        ../../utils/optionalFlags.dhall sha256:0dba774441dd92889f9a2a9819a6bca5ad7d1d891fbac9fa5c284367ca9fec33
      ? ../../utils/optionalFlags.dhall

let optionalOptions =
        ../../utils/optionalOptions.dhall sha256:51a1fbb37ceddab8f0ff0ef0fe2cd910af960bd447c6bcbc01b23426c0ee74f8
      ? ../../utils/optionalOptions.dhall

let Prelude =
        ../../../Prelude/package.dhall sha256:2086c6a2e7ce8ee371858036ab60d24d045969defadb4bc9d87639921ec2e028
      ? ../../../Prelude/package.dhall

let noArguments = [] : List Text

let toArguments =
      λ(defaultShakeVerbosity : ShakeVerbosity.Type) →
      λ(opts : Options.Type) →
          (if opts.noBuild then [ "--no-build" ] else noArguments)
        # optionalOptions
            Text
            (λ(dir : Text) → [ "--directory=${dir}" ])
            opts.directory
        # (if opts.colour then [ "--colour" ] else noArguments)
        # optionalOptions
            CompactOutput.Type
            ( λ(arg : CompactOutput.Type) →
                [ "--compact=${CompactOutput.toText arg}" ]
            )
            opts.compactOutput
        # optionalOptions
            Natural
            ( λ(n : Natural) →
                if    Prelude.Natural.isZero n
                then  [ "--jobs" ]
                else  [ "--jobs=${Prelude.Natural.show n}" ]
            )
            opts.jobs
        # optionalFlags [ "--keep-going" ] [ "--no-keep-going" ] opts.keepGoing
        # (if opts.skipCommands then [ "--skip-commands" ] else noArguments)
        # optionalOptions
            ShakeVerbosity.Type
            (ShakeVerbosity.toArguments defaultShakeVerbosity)
            opts.verbosity
        # optionalFlags
            [ "--print-directory" ]
            [ "--no-print-directory" ]
            opts.printDirectory
        # (if opts.noTime then [ "--no-time" ] else noArguments)
        # (if opts.timings then [ "--timings" ] else noArguments)

let test0 =
        assert
      :   toArguments
            ShakeVerbosity.default
            Options::{
            , noBuild = True
            , directory = Some "/path/to/directory/"
            , colour = True
            , compactOutput = Some CompactOutput.Type.Auto
            , jobs = Some 0
            , keepGoing = Some False
            }
        ≡ [ "--no-build"
          , "--directory=/path/to/directory/"
          , "--colour"
          , "--compact=auto"
          , "--jobs"
          , "--no-keep-going"
          ]

in    toArguments
    : ∀(defaultShakeVerbosity : ShakeVerbosity.Type) → Options.Type → List Text
