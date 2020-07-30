-- vim: filetype=dhall

let Options =
      { Type =
            ./Type.dhall sha256:888713faabcc80155c73e123229956496a179aeb6737d9c460107ab0b59c373f
          ? ./Type.dhall
      , default =
            ./default.dhall sha256:b81b593542b28171d9342252efa35f2d9b78625fefc073efb14be22c2e175454
          ? ./default.dhall
      }

let InputType =
        ../InputType/package.dhall sha256:c67f94e4f66a340f50cdcad06a2799521f6531865bb14dc14949962ed8217688
      ? ../InputType/package.dhall

let OutputType =
        ../OutputType/package.dhall sha256:0cf7ebdcb417d1df09580abdd209b2299d4d5bfa3dd174c0eeef7ee3612d1eb8
      ? ../OutputType/package.dhall

let Variable =
        ../Variable/package.dhall sha256:61c8e76b8bef4eaeb117df69184badbd12ed5a5eccc59c51123942dde3079338
      ? ../Variable/package.dhall

let OutputColour =
        ../OutputColour/package.dhall sha256:2297c975b893a070f00082bba345a64480cb073f59dc5155424829fadd4765af
      ? ../OutputColour/package.dhall

let Prelude =
        ../../prelude.dhall sha256:10db3c919c25e9046833df897a8ffe2701dc390fa0893d958c3430524be5a43e
      ? ../../prelude.dhall

let optionalOptions =
        ../../utils/optionalOptions.dhall sha256:51a1fbb37ceddab8f0ff0ef0fe2cd910af960bd447c6bcbc01b23426c0ee74f8
      ? ../../utils/optionalOptions.dhall

let noArguments = [] : List Text

let toArguments =
      λ(options : Options.Type) →
          optionalOptions InputType.Type InputType.toArguments options.inputType
        # optionalOptions
            OutputType.Type
            OutputType.toArguments
            options.outputType
        # (if options.asciiOutput then [ "--ascii-output" ] else noArguments)
        # ( if    options.compactOutput
            then  [ "--compact-output" ]
            else  noArguments
          )
        # (if options.sortKeys then [ "--sort-keys" ] else noArguments)
        # Prelude.List.concatMap
            Variable.Type
            Text
            Variable.toArguments
            options.variables
        # optionalOptions
            OutputColour.Type
            OutputColour.toArguments
            options.outputColour

let test0 = assert : toArguments Options::{=} ≡ noArguments

let test1 =
        assert
      :   toArguments
            Options::{
            , inputType = Some InputType.Type.Null
            , outputType = Some OutputType.Type.Raw
            , asciiOutput = True
            , compactOutput = True
            , sortKeys = True
            , outputColour = Some OutputColour.Type.Monochrome
            }
        ≡ [ "--null-input"
          , "--raw-output"
          , "--ascii-output"
          , "--compact-output"
          , "--sort-keys"
          , "--monochrome-output"
          ]

let test2 =
        assert
      :   toArguments
            Options::{
            , inputType = Some InputType.Type.Null
            , variables =
              [ { name = "some", value = Variable.Value.String "text" }
              , { name = "flags"
                , value =
                    Variable.Value.Json
                      (Prelude.JSON.array [ Prelude.JSON.bool True ])
                }
              ]
            }
        ≡ [ "--null-input"
          , "--arg"
          , "some"
          , "text"
          , "--argjson"
          , "flags"
          , ''
            [ true ]
            ''
          ]

in  toArguments : Options.Type → List Text
