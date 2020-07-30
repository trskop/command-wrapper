-- vim: filetype=dhall

let ExecCommand =
      { Type =
            ../../CommandWrapper/ExecCommand/Type sha256:6ece797a2c269b469da41f12ec2d7206846cac65183ae8213cce1b6d59f2b02b
          ? ../../CommandWrapper/ExecCommand/Type
      , default =
            ../../CommandWrapper/ExecCommand/default sha256:c3a088ca2b090c91d5d630c4e01f4b4fbd0136f5bb1251f6828698e5180685b2
          ? ../../CommandWrapper/ExecCommand/default
      }

let ColourOutput =
      { Type =
            ../../CommandWrapper/ColourOutput/Type sha256:0ef91a3f044406ee80fc20f26127b448a0e98f46c46ec024457023d2aded8543
          ? ../../CommandWrapper/ColourOutput/Type
      , toArgument =
            ../utils/colorOption.dhall sha256:1974a5a3f18b441b9eae2f7cea20168ab26a97d450ad85a9e6160cea8538a170
          ? ../utils/colorOption.dhall
      }

let Verbosity =
      { Type =
            ../../CommandWrapper/Verbosity/Type sha256:f009a39a49b1ee65651e9510778e7d72ff96820f4702a955e8f47682d72995c6
          ? ../../CommandWrapper/Verbosity/Type
      , fold =
            ../../CommandWrapper/Verbosity/fold sha256:4dac2c264a2531d569ad0e5f712a1cd2d17b51ecdc502cc72f19937bf4733b1e
          ? ../../CommandWrapper/Verbosity/fold
      }

let verbosityOption =
      Verbosity.fold
        (List Text)
        { Silent = [ "--silent" ]
        , Normal = [] : List Text
        , Verbose = [ "--verbosity=info" ]
        , Annoying = [ "--verbosity=debug" ]
        }

let Options =
      { Type =
            ./Options/Type.dhall sha256:b2526c288d5a0419d362c1375b879214396f6d1f6cd88b0416545724fd661b26
          ? ./Options/Type.dhall
      , toArguments =
            ./Options/toArguments.dhall sha256:378f10f92fda1dedf67914020e2bd332e1fff495b9a32dc15a1a01ef0e82a30b
          ? ./Options/toArguments.dhall
      }

in  λ(options : Options.Type) →
    λ(args : List Text) →
    λ(verbosity : Verbosity.Type) →
    λ(colourOutput : ColourOutput.Type) →
    λ(arguments : List Text) →
      ExecCommand::{
      , command = "stack"
      , arguments =
            verbosityOption verbosity
          # [ ColourOutput.toArgument colourOutput ]
          # Options.toArguments options
          # args
          # arguments
      , workingDirectory = options.workingDirectory
      }
