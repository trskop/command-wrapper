-- vim: filetype=dhall

let Options =
      { Type =
            ./Type.dhall sha256:54460863b32e729cf617282abfc10164e5ac65565bb8b71099b27567e6404a3d
          ? ./Type.dhall
      , default =
            ./default.dhall sha256:e7779cc66f4072ec1586cc99158ee19641c282c3c03d6d42363876695c5af8ec
          ? ./default.dhall
      }

let optionalOptions =
        ../../utils/optionalOptions.dhall sha256:51a1fbb37ceddab8f0ff0ef0fe2cd910af960bd447c6bcbc01b23426c0ee74f8
      ? ../../utils/optionalOptions.dhall

let optionalFlags =
        ../../utils/optionalFlags.dhall sha256:0dba774441dd92889f9a2a9819a6bca5ad7d1d891fbac9fa5c284367ca9fec33
      ? ../../utils/optionalFlags.dhall

let Prelude =
        ../../prelude.dhall sha256:10db3c919c25e9046833df897a8ffe2701dc390fa0893d958c3430524be5a43e
      ? ../../prelude.dhall

let noArguments = [] : List Text

let PreviewWindowPosition = < Up | Down | Left | Right >

let Size = < Lines : Natural | Percentage : Natural >

let PreviewWindowOptions =
      { position : Optional PreviewWindowPosition
      , size : Optional Size
      , wrapLines : Bool
      , startHidden : Bool
      }

let PreviewWindowOptions/toArguments =
      λ(options : PreviewWindowOptions) →
        let position =
              Prelude.Optional.fold
                PreviewWindowPosition
                options.position
                Text
                ( λ(_ : PreviewWindowPosition) →
                    merge
                      { Up = "up"
                      , Down = "down"
                      , Left = "left"
                      , Right = "right"
                      }
                      _
                )
                ""

        let havePosition =
              Prelude.Bool.not
                (Prelude.Optional.null PreviewWindowPosition options.position)

        let haveSize =
              Prelude.Bool.not (Prelude.Optional.null Size options.size)

        let size =
              Prelude.Optional.fold
                Size
                options.size
                Text
                ( λ(_ : Size) →
                        (if havePosition then "" else ":")
                    ++  merge
                          { Lines = λ(n : Natural) → Natural/show n
                          , Percentage = λ(n : Natural) → Natural/show n ++ "%"
                          }
                          _
                )
                ""

        let wrapLines =
              if    options.wrapLines
              then  (if havePosition || haveSize then ":" else "") ++ "wrap"
              else  ""

        let startHidden =
              if    options.startHidden
              then      ( if    havePosition || haveSize || options.wrapLines
                          then  ":"
                          else  ""
                        )
                    ++  "hidden"
              else  ""

        in  if        havePosition
                  ||  haveSize
                  ||  options.wrapLines
                  ||  options.startHidden
            then  [ "--preview-window=${position}${size}${wrapLines}${startHidden}"
                  ]
            else  noArguments

let optionalFlag =
      λ(flagName : Text) →
      λ(flag : Optional Bool) →
        optionalFlags [ "--${flagName}" ] [ "--no-${flagName}" ] flag

let toArguments =
      λ(_ : Options.Type) →
          optionalFlag "extended" _.extended
        # optionalFlag "exact" _.exactMatch
        # optionalOptions
            < v1 | v2 >
            ( λ(v : < v1 | v2 >) →
                merge { v1 = [ "--algo=v1" ], v2 = [ "--algo=v2" ] } v
            )
            _.algorithm
        # merge
            { SmartCase = noArguments
            , CaseInsensitive = [ "-i" ]
            , CaseSensitive = [ "+i" ]
            }
            _.caseSensitive
        # optionalOptions
            Text
            (λ(t : Text) → [ "--delimiter=${t}" ])
            _.delimiter
        # optionalFlag "sort" _.sortResult
        # optionalFlag "tac" _.reverseInputOrder
        # ( if    Prelude.List.null
                    < Length | Begin | End | Index >
                    _.tiebreakSortCriteria
            then  noArguments
            else  [     "--tiebreak="
                    ++  Prelude.Text.concatMapSep
                          ","
                          < Length | Begin | End | Index >
                          ( λ(c : < Length | Begin | End | Index >) →
                              merge
                                { Length = "length"
                                , Begin = "begin"
                                , End = "end"
                                , Index = "index"
                                }
                                c
                          )
                          _.tiebreakSortCriteria
                  ]
          )
        # optionalFlag "multi" _.multiSelect
        # (if _.noMouse then [ "--no-mouse" ] else noArguments)
        # optionalFlag "cycle" _.cycleScroll
        # optionalFlag "filepath-word" _.respectFilepathSeparator
        # optionalOptions
            Size
            ( λ(height : Size) →
                let value =
                      merge
                        { Lines = λ(n : Natural) → Natural/show n
                        , Percentage = λ(n : Natural) → Natural/show n ++ "%"
                        }
                        height

                in  [ "--height=${value}" ]
            )
            _.height
        # optionalOptions
            Natural
            (λ(height : Natural) → [ "--min-height=${Natural/show height}" ])
            _.minHeight
        # optionalOptions
            < BottomOfTheScreen
            | TopOfTheScreen
            | TopOfTheScreenPromptAtTheBottom
            >
            ( λ ( layout
                : < BottomOfTheScreen
                  | TopOfTheScreen
                  | TopOfTheScreenPromptAtTheBottom
                  >
                ) →
                let value =
                      merge
                        { BottomOfTheScreen = "default"
                        , TopOfTheScreen = "reverse"
                        , TopOfTheScreenPromptAtTheBottom = "reverse-list"
                        }
                        layout

                in  [ "--layout=${value}" ]
            )
            _.layout
        # optionalFlag "border" _.drawBorder
        # optionalFlag "unicode" _.useUnicodeCharacters
        # optionalFlag "inline-info" _.inlineFinderInfo
        # optionalOptions Text (λ(str : Text) → [ "--prompt=${str}" ]) _.prompt
        # optionalOptions Text (λ(str : Text) → [ "--header=${str}" ]) _.header
        # optionalOptions
            Natural
            (λ(n : Natural) → [ "--header-lines=${Natural/show n}" ])
            _.headerLines
        # optionalFlag "ansi" _.interpretAnsiCodes
        # optionalFlag "bold" _.useBoldText
        # optionalFlag "black" _.blackBackground
        # optionalOptions
            Text
            (λ(file : Text) → [ "--history=${file}" ])
            _.historyFile
        # optionalOptions
            Natural
            (λ(n : Natural) → [ "--history-size=${Natural/show n}" ])
            _.historySize
        # optionalOptions
            Text
            (λ(command : Text) → [ "--preview=${command}" ])
            _.previewCommand
        # PreviewWindowOptions/toArguments _.previewWindowOptions
        # optionalOptions Text (λ(str : Text) → [ "--query=${str}" ]) _.query
        # optionalFlag "select-1" _.select1
        # optionalFlag "exit-0" _.exit0
        # optionalOptions Text (λ(str : Text) → [ "--filter=${str}" ]) _.filter
        # optionalFlag "print-query" _.printQuery
        # ( if    Prelude.List.null Text _.expectKeys
            then  noArguments
            else  [ "--expect=${Prelude.Text.concatSep "," _.expectKeys}" ]
          )
        # optionalFlag "read0" _.read0
        # optionalFlag "print0" _.print0
        # optionalFlag "sync" _.synchronous

let noArgumentsByDefault = assert : toArguments Options.default ≡ noArguments

in  toArguments
