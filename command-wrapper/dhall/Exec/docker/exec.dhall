-- vim: filetype=dhall
--
-- Transform a command into:
--
-- ```
-- docker [OPTIONS] exec [EXEC_OPTIONS] CONTAINER COMMAND [ARGUMENTS]
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
        ./Options/package.dhall sha256:b801edf6d8ea64eaad3d358bd788a378a58bd20a167cb8bde32d4fd51b9d1b72
      ? ./Options/package.dhall

let ExecOptions =
        ./ExecOptions/package.dhall sha256:227af5c4f3ea7a7f05c560775e8ef911e8952f6ce796de762b8d57fe48e7c531
      ? ./ExecOptions/package.dhall

let docker =
        ./command.dhall sha256:058e3cb1f331261410ce12af3b037d76ced180788a86320432bf8d5f5bd6415c
      ? ./command.dhall

let -- Same function is in `run.dhall`, modulo types.
    amendExecOptions =
      λ(execOptions : ExecOptions.Type) →
      λ(command : ExecCommand.Type) →
        let -- Reason why execOptions.workingDirectory has precedence is that we
            -- may want to override workind directory of the inner command which
            -- may have been originally designed to run outside of a container.
            workingDirectory =
              merge
                { None = command.workingDirectory
                , Some = λ(dir : Text) → Some dir
                }
                execOptions.workingDirectory

        let -- We assume that the command needs environment variables that were
            -- defined in there. This may become tricky if we need to override
            -- them for the use inside container. Unfortunately there's no way
            -- how to handle that case gracefully.
            environment =
              execOptions.environment # command.environment

        in  execOptions ⫽ { environment, workingDirectory }

let command =
      λ(options : Options.Type) →
      λ(unalteredExecOptions : ExecOptions.Type) →
      λ(container : Text) →
      λ ( mkCommand
        : Verbosity → ColourOutput → ∀(arguments : List Text) → ExecCommand.Type
        ) →
      λ(verbosity : Verbosity) →
      λ(colourOutput : ColourOutput) →
      λ(arguments : List Text) →
        let command = mkCommand verbosity colourOutput arguments

        let execOptions = amendExecOptions unalteredExecOptions command

        let execArguments =
                [ "exec" ]
              # ExecOptions.toArguments execOptions
              # [ container, command.command ]
              # command.arguments

        in  docker options execArguments verbosity colourOutput ([] : List Text)

let test0 =
        assert
      :   command
            Options::{=}
            ExecOptions::{ interactive = True, allocateTty = True }
            "postgresql"
            ( λ(_ : Verbosity) →
              λ(_ : ColourOutput) →
              λ(arguments : List Text) →
                ExecCommand::{
                , command = "pg_dump"
                , arguments = [ "--username=admin" ] # arguments
                , environment =
                  [ { name = "PGPASSFILE", value = "/tmp/pgpass" } ]
                }
            )
            Verbosity.Normal
            ColourOutput.Auto
            [ "--dbname=events" ]
        ≡ ExecCommand::{
          , command = "docker"
          , arguments =
            [ "exec"
            , "--interactive"
            , "--tty"
            , "--env"
            , "PGPASSFILE=/tmp/pgpass"
            , "postgresql"
            , "pg_dump"
            , "--username=admin"
            , "--dbname=events"
            ]
          }

in    command
    : Options.Type →
      ExecOptions.Type →
      ∀(container : Text) →
      ∀(mkCommand : Verbosity → ColourOutput → List Text → ExecCommand.Type) →
      Verbosity →
      ColourOutput →
      ∀(arguments : List Text) →
        ExecCommand.Type
