-- vim: filetype=dhall

let Options =
      { Type =
            ./Type.dhall sha256:61553461550695b5b8c52861fff9ceecf0c70ebd703cfa2f7465d97aca6d3a66
          ? ./Type.dhall
      , default =
            ./default.dhall sha256:e105968e9739efb82a2a48c7bf6e15949e1d0b511dca30e2e2deb5a16e76a1fb
          ? ./default.dhall
      }

let optionalOptions =
        ../../utils/optionalOptions.dhall sha256:51a1fbb37ceddab8f0ff0ef0fe2cd910af960bd447c6bcbc01b23426c0ee74f8
      ? ../../utils/optionalOptions.dhall

let noArguments = [] : List Text

let Verbosity =
      let Verbosity = < default | verbose | terse >
      
      in  { Type = Verbosity
          , toArguments =
              λ(_ : Verbosity) →
                [     "--set=VERBOSITY="
                  ++  merge
                        { default = "default"
                        , verbose = "verbose"
                        , terse = "terse"
                        }
                        _
                ]
          }

let ShowContext =
      let ShowContext = < always | errors | never >
      
      in  { Type = ShowContext
          , toArguments =
              λ(_ : ShowContext) →
                [     "--set=SHOW_CONTEXT="
                  ++  merge
                        { always = "always"
                        , errors = "errors"
                        , never = "never"
                        }
                        _
                ]
          }

let toArguments =
      λ(options : Options.Type) →
          optionalOptions Verbosity.Type Verbosity.toArguments options.verbosity
        # optionalOptions
            ShowContext.Type
            ShowContext.toArguments
            options.showContext
        # (if options.quiet then [ "--quiet" ] else noArguments)

let test0 = assert : toArguments Options::{=} ≡ noArguments

let test1 =
        assert
      :   toArguments
            Options::{
            , pgpassFile = Some "/path/to/a/pgpass/file"
            , quiet = True
            , verbosity = Some Verbosity.Type.verbose
            , showContext = Some ShowContext.Type.always
            }
        ≡ [ "--set=VERBOSITY=verbose", "--set=SHOW_CONTEXT=always", "--quiet" ]

in  toArguments : Options.Type → List Text
