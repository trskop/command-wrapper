-- vim: filetype=dhall

let Options =
      { Type =
            ./Type.dhall sha256:674f9d48d01ccb653cd071dd30af0766337d30d0c8fa11498ab3a6e91219479a
          ? ./Type.dhall
      , default =
            ./default.dhall sha256:6c7eb164746af8d0c1106285fdb390265b46ae49928384734675638088522e32
          ? ./default.dhall
      }

let optionalOptions =
        ../../utils/optionalOptions.dhall sha256:51a1fbb37ceddab8f0ff0ef0fe2cd910af960bd447c6bcbc01b23426c0ee74f8
      ? ../../utils/optionalOptions.dhall

let verbosityOptions =
        ../../utils/verbosityOptions.dhall sha256:53abdd9ed8f27c0d175efc6b33e0a72d1d77661554e3e79e2a23d2c1252aa9a9
      ? ../../utils/verbosityOptions.dhall

let ColourOutput =
        ../../../CommandWrapper/ColourOutput/Type sha256:0ef91a3f044406ee80fc20f26127b448a0e98f46c46ec024457023d2aded8543
      ? ../../../CommandWrapper/ColourOutput/Type

let Verbosity =
        ../../../CommandWrapper/Verbosity/Type sha256:f009a39a49b1ee65651e9510778e7d72ff96820f4702a955e8f47682d72995c6
      ? ../../../CommandWrapper/Verbosity/Type

let noArguments = [] : List Text

let toArguments =
      λ(options : Options.Type) →
      λ(verbosity : Optional Verbosity) →
      λ(_ : Optional ColourOutput) →
          (if options.noPager then [ "--nopager" ] else noArguments)
        # (if options.noRun then [ "--norun" ] else noArguments)
        # optionalOptions
            Verbosity
            ( verbosityOptions
                { Silent = noArguments
                , Normal = noArguments
                , Verbose = noArguments
                , Annoying = [ "--debug" ]
                }
            )
            verbosity

let test0 =
        assert
      :   toArguments Options::{=} (None Verbosity) (None ColourOutput)
        ≡ noArguments

let test0 =
        assert
      :   toArguments
            Options::{
            , noPager = True
            , noRun = True
            , workingDirectory = Some "/path/to/a/directory"
            }
            (None Verbosity)
            (None ColourOutput)
        ≡ [ "--nopager", "--norun" ]

in    toArguments
    : Options.Type → Optional Verbosity → Optional ColourOutput → List Text
