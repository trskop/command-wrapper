-- vim: filetype=dhall
--
-- ```
-- ssh [OPTIONS] [-p PORT] [EXTRA_OPTIONS] [user@]host -- [ARGUMENTS]
-- ```

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
        ../../CommandWrapper/Verbosity/Type sha256:f009a39a49b1ee65651e9510778e7d72ff96820f4702a955e8f47682d72995c6
      ? ../../CommandWrapper/Verbosity/Type

let Options =
        ./Options/package.dhall sha256:c9ec017070918c472f09b7ca65d505de2855d90fb600aafb2988b4f5f2738d73
      ? ./Options/package.dhall

let SshTo =
        ./SshTo/package.dhall sha256:67545f844076e4c3491d6d76bce30e54fa5b8e8e5fcf35788145e61a93a7b2be
      ? ./SshTo/package.dhall

let optionalOptions =
        ../utils/optionalOptions.dhall sha256:51a1fbb37ceddab8f0ff0ef0fe2cd910af960bd447c6bcbc01b23426c0ee74f8
      ? ../utils/optionalOptions.dhall

let command =
      λ(options : Options.Type) →
      λ(sshTo : SshTo.Type) →
      λ(extraOptions : List Text) →
      λ(verbosity : Verbosity) →
      λ(colourOutput : ColourOutput) →
      λ(arguments : List Text) →
        ExecCommand::{
        , command = "ssh"
        , arguments =
              Options.toArguments options
            # optionalOptions
                Natural
                (λ(_ : Natural) → [ "-p", Natural/show _ ])
                sshTo.port
            # extraOptions
            # [ merge
                  { None = sshTo.host
                  , Some = λ(_ : Text) → "${_}@${sshTo.host}"
                  }
                  sshTo.user
              , "--"
              ]
            # arguments
        , environment = Options.toEnvironment options
        , workingDirectory = options.workingDirectory
        }

let test0 =
        assert
      :   command
            Options::{=}
            SshTo::{ host = "work-machine" }
            ([] : List Text)
            Verbosity.Normal
            ColourOutput.Auto
            ([] : List Text)
        ≡ ExecCommand::{ command = "ssh", arguments = [ "work-machine", "--" ] }

let test1 =
        assert
      :   command
            Options::{=}
            SshTo::{
            , port = Some 443
            , user = Some "admin"
            , host = "work-machine"
            }
            ([] : List Text)
            Verbosity.Normal
            ColourOutput.Auto
            ([] : List Text)
        ≡ ExecCommand::{
          , command = "ssh"
          , arguments = [ "-p", "443", "admin@work-machine", "--" ]
          }

in    command
    : Options.Type →
      SshTo.Type →
      ∀(extraOptions : List Text) →
      Verbosity →
      ColourOutput →
      ∀(arguments : List Text) →
        ExecCommand.Type
