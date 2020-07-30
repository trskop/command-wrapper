-- vim: filetype=dhall
--
-- ```
-- TOOLSET completion --wrapper --expression=EXPRESSION --exec -- --index=INDEX
--     --shell=SHELL -- COMMAND [PREFIX_ARGUMENT ...] [WORD ...]
-- ```

let ExecCommand =
      { Type =
            ../../CommandWrapper/ExecCommand/Type sha256:6ece797a2c269b469da41f12ec2d7206846cac65183ae8213cce1b6d59f2b02b
          ? ../../CommandWrapper/ExecCommand/Type
      , default =
            ../../CommandWrapper/ExecCommand/default sha256:c3a088ca2b090c91d5d630c4e01f4b4fbd0136f5bb1251f6828698e5180685b2
          ? ../../CommandWrapper/ExecCommand/default
      }

let Shell =
      { Type =
            ../../CommandWrapper/Shell/Type sha256:f61ef033bfb850ef4bf0c3d0d18c69d1b15b38cc9e3df4a1abea334b89ed5555
          ? ../../CommandWrapper/Shell/Type
      , toText =
            ../../CommandWrapper/Shell/toText sha256:ac2b3762009ee945aac998034d4b44cb47fff0593df4547c9423dd62e54e92d4
          ? ../../CommandWrapper/Shell/toText
      }

let Prelude =
        ../prelude.dhall sha256:10db3c919c25e9046833df897a8ffe2701dc390fa0893d958c3430524be5a43e
      ? ../prelude.dhall

let mkArguments =
      λ(shell : Shell.Type) →
      λ(index : Natural) →
      λ(words : List Text) →
          [ "--index=${Natural/show index}"
          , "--shell=${Shell.toText shell}"
          , "--"
          ]
        # words

let wrapper =
      λ(options : { toolset : Text, expression : Text, command : Text }) →
      λ(workingDirectory : Optional Text) →
      λ(prefixArguments : List Text) →
      λ(shell : Shell.Type) →
      λ(index : Natural) →
      λ(words : List Text) →
        ExecCommand::{
        , command = options.toolset
        , arguments =
            let adjusted-index =
                  index + Prelude.List.length Text prefixArguments

            let arguments =
                  mkArguments
                    shell
                    adjusted-index
                    ([ options.command ] # prefixArguments # words)

            in    [ "--no-aliases"
                  , "--silent"
                  , "completion"
                  , "--wrapper"
                  , "--expression=${options.expression}"
                  , "--exec"
                  , "--"
                  ]
                # arguments
        , workingDirectory
        }

in    wrapper
    : ∀(options : { toolset : Text, expression : Text, command : Text }) →
      ∀(workingDirectory : Optional Text) →
      ∀(prefixArguments : List Text) →
      Shell.Type →
      ∀(index : Natural) →
      ∀(words : List Text) →
        ExecCommand.Type
