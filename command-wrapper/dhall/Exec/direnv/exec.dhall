-- vim: filetype=dhall

let ExecCommand =
        ../../CommandWrapper/ExecCommand/Type sha256:6ece797a2c269b469da41f12ec2d7206846cac65183ae8213cce1b6d59f2b02b
      ? ../../CommandWrapper/ExecCommand/Type

let ColourOutput =
        ../../CommandWrapper/ColourOutput/Type sha256:0ef91a3f044406ee80fc20f26127b448a0e98f46c46ec024457023d2aded8543
      ? ../../CommandWrapper/ColourOutput/Type

let Verbosity =
        ../../CommandWrapper/Verbosity/Type sha256:f009a39a49b1ee65651e9510778e7d72ff96820f4702a955e8f47682d72995c6
      ? ../../CommandWrapper/Verbosity/Type

let Options =
        ./Options/package.dhall sha256:f40d104b121ea8568116570f0eebafc8853c7983ef3e9efcd40ad9326a92e481
      ? ./Options/package.dhall

let direnv =
        ./command.dhall sha256:e9c3b8f1b0a69c31bd7d6d4efd9f41144b03b3885c37b09b907a5a66beff2387
      ? ./command.dhall

let optionalOptions =
        ../utils/optionalOptions.dhall sha256:51a1fbb37ceddab8f0ff0ef0fe2cd910af960bd447c6bcbc01b23426c0ee74f8
      ? ../utils/optionalOptions.dhall

let command =
      λ(options : Options.Type) →
      λ(mkCommand : Verbosity → ColourOutput → List Text → ExecCommand) →
      λ(verbosity : Verbosity) →
      λ(colourOutput : ColourOutput) →
      λ(extraArguments : List Text) →
        direnv
          options
          ( let dir =
                  optionalOptions
                    Text
                    (λ(d : Text) → [ d ])
                    options.workingDirectory

            let cmd = mkCommand verbosity colourOutput extraArguments

            in  [ "exec" ] # dir # [ cmd.command ] # cmd.arguments
          )
          verbosity
          colourOutput
          ([] : List Text)

in    command
    : Options.Type →
      ∀(mkCommand : Verbosity → ColourOutput → List Text → ExecCommand) →
      Verbosity →
      ColourOutput →
      ∀(extraArguments : List Text) →
        ExecCommand
