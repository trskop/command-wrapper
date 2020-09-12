-- vim: filetype=dhall

let Options =
      { Type =
            ./Type.dhall sha256:9c93aeda7d25765472107935265ba78477d16d1787b33f0d6db56d27174bb532
          ? ./Type.dhall
      , default =
            ./default.dhall sha256:49a9f1ae25424574c1694747822da1b8627010c47f18d495e197a9ca36c317b7
          ? ./default.dhall
      }

let optionalOptions =
        ../../utils/optionalOptions.dhall sha256:51a1fbb37ceddab8f0ff0ef0fe2cd910af960bd447c6bcbc01b23426c0ee74f8
      ? ../../utils/optionalOptions.dhall

let optionalFlags =
        ../../utils/optionalFlags.dhall sha256:0dba774441dd92889f9a2a9819a6bca5ad7d1d891fbac9fa5c284367ca9fec33
      ? ../../utils/optionalFlags.dhall

let Prelude =
        ../../../Prelude/package.dhall sha256:2086c6a2e7ce8ee371858036ab60d24d045969defadb4bc9d87639921ec2e028
      ? ../../../Prelude/package.dhall

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

let boolFlag =
      λ(flagName : Text) →
      λ(flag : Bool) →
        if flag then [ "--${flagName}" ] else noArguments

let toArguments =
      λ(_ : Options.Type) →
          boolFlag "exact" _.exactMatch
        # boolFlag "regex" _.regex
        # optionalOptions
            Text
            (λ(t : Text) → [ "--delimiter=${t}" ])
            _.delimiter
        # boolFlag "tac" _.reverseInputOrder
        # ( if    Prelude.List.null
                    < Score | Index | Begin | End >
                    _.tiebreakSortCriteria
            then  noArguments
            else  [     "--tiebreak="
                    ++  Prelude.Text.concatMapSep
                          ","
                          < Score | Index | Begin | End >
                          ( λ(c : < Score | Index | Begin | End >) →
                              merge
                                { Score = "score"
                                , Index = "index"
                                , Begin = "begin"
                                , End = "end"
                                }
                                c
                          )
                          _.tiebreakSortCriteria
                  ]
          )
        # boolFlag "interactive" _.interactive
        # optionalOptions Text (λ(t : Text) → [ "--cmd=${t}" ]) _.command
        # optionalFlag "multi" _.multiSelect
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
        # boolFlag "inline-info" _.inlineFinderInfo
        # optionalOptions Text (λ(str : Text) → [ "--prompt=${str}" ]) _.prompt
        # optionalOptions
            Text
            (λ(str : Text) → [ "--cmd-prompt=${str}" ])
            _.cmdPrompt
        # optionalOptions Text (λ(str : Text) → [ "--header=${str}" ]) _.header
        # optionalOptions
            Natural
            (λ(n : Natural) → [ "--header-lines=${Natural/show n}" ])
            _.headerLines
        # boolFlag "ansi" _.interpretAnsiCodes
        # optionalOptions
            Natural
            (λ(n : Natural) → [ "--tabstop=${Natural/show n}" ])
            _.tabstop
        # optionalOptions
            Text
            (λ(command : Text) → [ "--preview=${command}" ])
            _.previewCommand
        # PreviewWindowOptions/toArguments _.previewWindowOptions
        # optionalOptions Text (λ(str : Text) → [ "--query=${str}" ]) _.query
        # optionalOptions
            Text
            (λ(str : Text) → [ "--cmd-query=${str}" ])
            _.cmdQuery
        # boolFlag "print-query" _.printQuery
        # optionalOptions Text (λ(str : Text) → [ "--filter=${str}" ]) _.filter
        # ( if    Prelude.List.null Text _.expectKeys
            then  noArguments
            else  [ "--expect=${Prelude.Text.concatSep "," _.expectKeys}" ]
          )
        # boolFlag "read0" _.read0
        # boolFlag "print0" _.print0

let noArgumentsByDefault = assert : toArguments Options::{=} ≡ noArguments

in  toArguments : Options.Type → List Text
