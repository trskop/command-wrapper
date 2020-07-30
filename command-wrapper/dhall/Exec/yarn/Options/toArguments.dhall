-- vim: filetype=dhall

let Options =
      { Type =
            ./Type.dhall sha256:4a4dda0eaf771745c5bfdefb80cdf489c87385626c9501ee19e00485bca4b48f
          ? ./Type.dhall
      , default =
            ./default.dhall sha256:e5843167eab527a6c37c556528ec5ffedb1ba0bf2f0e5f39001b1b1718a3cf1d
          ? ./default.dhall
      }

let optionalOptions =
        ../../utils/optionalOptions.dhall sha256:51a1fbb37ceddab8f0ff0ef0fe2cd910af960bd447c6bcbc01b23426c0ee74f8
      ? ../../utils/optionalOptions.dhall

let YarnVerbosity =
      let YarnVerbosity = { Type = < silent | verbose > }
      
      in    YarnVerbosity
          ∧ { toArguments =
                λ(_ : YarnVerbosity.Type) →
                  [ merge { silent = "--silent", verbose = "--verbose" } _ ]
            }

let noArguments = [] : List Text

let toArguments =
      λ(opts : Options.Type) →
          optionalOptions
            YarnVerbosity.Type
            YarnVerbosity.toArguments
            opts.verbosity
        # optionalOptions
            Text
            (λ(_ : Text) → [ "--cwd=${_}" ])
            opts.workingDirectory

let test0 = assert : toArguments Options::{=} ≡ noArguments

let test1 =
        assert
      :   toArguments
            Options::{
            , verbosity = Some YarnVerbosity.Type.verbose
            , workingDirectory = Some "/path/to/a/directory"
            }
        ≡ [ "--verbose", "--cwd=/path/to/a/directory" ]

in  toArguments : Options.Type → List Text
