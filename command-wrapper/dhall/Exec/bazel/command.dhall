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
        ../../CommandWrapper/ColourOutput/Type sha256:0ef91a3f044406ee80fc20f26127b448a0e98f46c46ec024457023d2aded8543
      ? ../../CommandWrapper/ColourOutput/Type

let Verbosity =
      { Type =
            ../../CommandWrapper/Verbosity/Type sha256:f009a39a49b1ee65651e9510778e7d72ff96820f4702a955e8f47682d72995c6
          ? ../../CommandWrapper/Verbosity/Type
      , fold =
            ../../CommandWrapper/Verbosity/fold sha256:4dac2c264a2531d569ad0e5f712a1cd2d17b51ecdc502cc72f19937bf4733b1e
          ? ../../CommandWrapper/Verbosity/fold
      }

let colorOption =
        ../utils/colorOption.dhall sha256:1974a5a3f18b441b9eae2f7cea20168ab26a97d450ad85a9e6160cea8538a170
      ? ../utils/colorOption.dhall

let List/head-and-tail =
        ../../CommandWrapper/List/headAndTail sha256:06d3b92abb0790387092bf6db0ff9fc20299a898bb9e9a8e34373ad92b5ed86d
      ? ../../CommandWrapper/List/headAndTail

let optionalOptions =
        ../utils/optionalOptions.dhall sha256:51a1fbb37ceddab8f0ff0ef0fe2cd910af960bd447c6bcbc01b23426c0ee74f8
      ? ../utils/optionalOptions.dhall

let verbosityOptions =
      Verbosity.fold
        (List Text)
        { Silent = [ "--logging=0" ]
        , Normal = [] : List Text
        , Verbose = [ "--logging=5", "--explain" ]
        , Annoying =
          [ "--logging=6"
          , "--explain"
          , "--verbose_explanations"
          , "--verbose_failures"
          ]
        }

let command =
      λ(workingDirectory : Optional Text) →
      λ(args : List Text) →
      λ(verbosity : Verbosity.Type) →
      λ(colourOutput : ColourOutput) →
      λ(extraArgs : List Text) →
        ExecCommand::{
        , command = "bazel"
        , arguments =
            let arguments = List/head-and-tail Text (args # extraArgs)
            
            in    verbosityOptions verbosity
                # optionalOptions
                    Text
                    (λ(cmd : Text) → [ cmd, colorOption colourOutput ])
                    arguments.head
                # arguments.tail
        , workingDirectory
        }

in    command
    : ∀(workingDirectory : Optional Text) →
      ∀(args : List Text) →
      Verbosity.Type →
      ColourOutput →
      ∀(extraArgs : List Text) →
        ExecCommand.Type