-- vim: filetype=dhall

let Options =
      { Type =
            ./Type.dhall sha256:7cbb3d15d2d57539fe0274c8c1223148bb7b34d0310e0c58d4ab7f49e7e0ca5f
          ? ./Type.dhall
      , default =
            ./default.dhall sha256:fcc7c0bfd22e1c1cc54cfe6249340f56410e296a90da6720156af78b7b7365ca
          ? ./default.dhall
      }

let Verbosity =
        ../../../CommandWrapper/Verbosity/Type sha256:f009a39a49b1ee65651e9510778e7d72ff96820f4702a955e8f47682d72995c6
      ? ../../../CommandWrapper/Verbosity/Type

let Verbosity/fold =
        ../../../CommandWrapper/Verbosity/fold sha256:4dac2c264a2531d569ad0e5f712a1cd2d17b51ecdc502cc72f19937bf4733b1e
      ? ../../../CommandWrapper/Verbosity/fold

let ColourOutput =
        ../../../CommandWrapper/ColourOutput/Type sha256:0ef91a3f044406ee80fc20f26127b448a0e98f46c46ec024457023d2aded8543
      ? ../../../CommandWrapper/ColourOutput/Type

let ColourOutput/toText =
        ../../../CommandWrapper/ColourOutput/toText sha256:51d22acdc9b32f757e9170ae7c2cf244ce0488aa839dada9ee38bb3f1ee4d7bf
      ? ../../../CommandWrapper/ColourOutput/toText

let optionalOptions =
        ../../utils/optionalOptions.dhall sha256:51a1fbb37ceddab8f0ff0ef0fe2cd910af960bd447c6bcbc01b23426c0ee74f8
      ? ../../utils/optionalOptions.dhall

let optionalFlags =
        ../../utils/optionalFlags.dhall sha256:0dba774441dd92889f9a2a9819a6bca5ad7d1d891fbac9fa5c284367ca9fec33
      ? ../../utils/optionalFlags.dhall

let noArguments = [] : List Text

let toArguments =
      λ(options : Options.Type) →
          optionalOptions
            Verbosity
            ( λ(verbosity : Verbosity) →
                [     "--verbosity="
                  ++  Verbosity/fold
                        Text
                        { Silent = "silent"
                        , Normal = "normal"
                        , Verbose = "verbose"
                        , Annoying = "annoying"
                        }
                        verbosity
                ]
            )
            options.verbosity
        # optionalOptions
            ColourOutput
            ( λ(colour : ColourOutput) →
                [ "--colour=${ColourOutput/toText colour}" ]
            )
            options.colour
        # optionalFlags [ "--aliases" ] [ "--no-aliases" ] options.allowAliases
        # optionalOptions
            Text
            (λ(directory : Text) → [ "--change-directory=${directory}" ])
            options.changeDirectory

let -- Default value should not pass any options to underlying command.
    noArgumentsByDefault =
      assert : toArguments Options.default ≡ noArguments

in  toArguments : Options.Type → List Text
