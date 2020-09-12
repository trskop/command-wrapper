-- vim: filetype=dhall
--
-- Convert output of `TOOLSET exec --print COMMAND` into command that can be
-- executed on command line.
--
-- This script can be used in multiple ways, here are some of them:
--
-- ```
-- "${TOOLSET_COMMAND}" exec --print "${COMMAND}" > command.dhall
-- "${TOOLSET_COMMAND}" config --dhall-text <<< './toShell ./command.dhall'
-- ```
--
-- ```
-- export EXEC_COMMAND="$("${TOOLSET_COMMAND}" exec --print "${COMMAND}")"
-- "${TOOLSET_COMMAND}" config --dhall-text <<< './toShell env:EXEC_COMMAND'
-- ```
--
-- ```
-- "${TOOLSET_COMMAND}" exec --print "${COMMAND}" \
-- | "${TOOLSET_COMMAND}" config --dhall-filter './toShell input' \
-- | "${TOOLSET_COMMAND}" config --dhall-text
-- ```

let ExecCommand =
      { Type =
            ../../CommandWrapper/ExecCommand/Type sha256:6ece797a2c269b469da41f12ec2d7206846cac65183ae8213cce1b6d59f2b02b
          ? ../../CommandWrapper/ExecCommand/Type
      , default =
            ../../CommandWrapper/ExecCommand/default sha256:c3a088ca2b090c91d5d630c4e01f4b4fbd0136f5bb1251f6828698e5180685b2
          ? ../../CommandWrapper/ExecCommand/default
      }

let EnvironmentVariable =
      { Type =
            ../../CommandWrapper/EnvironmentVariable/Type sha256:b8c3c0c4ceb36ba4e6674df5de20ad1d97e120b93b9ce9914a41d0036770dcc4
          ? ../../CommandWrapper/EnvironmentVariable/Type
      , toText =
            ../../CommandWrapper/EnvironmentVariable/toText sha256:14b6ceff46e7ec8cd1846d2f50ba366efb3e623f6d92e7543aabaab1ac265f34
          ? ../../CommandWrapper/EnvironmentVariable/toText
      }

let Prelude =
        ../../Prelude/package.dhall sha256:2086c6a2e7ce8ee371858036ab60d24d045969defadb4bc9d87639921ec2e028
      ? ../../Prelude/package.dhall

let withWorkingDirectory =
      λ(dir : Optional Text) →
      λ(cmd : Text) →
        merge
          { None = cmd, Some = λ(_ : Text) → "( cd ${Text/show _} && ${cmd} )" }
          dir

let toShell =
      λ(cmd : ExecCommand.Type) →
            withWorkingDirectory
              cmd.workingDirectory
              (     ( if    Prelude.List.null
                              EnvironmentVariable.Type
                              cmd.environment
                      then  ""
                      else  "env "
                    )
                ++  Prelude.Text.concatMapSep
                      " "
                      Text
                      Text/show
                      (   Prelude.List.map
                            EnvironmentVariable.Type
                            Text
                            EnvironmentVariable.toText
                            cmd.environment
                        # [ cmd.command ]
                        # cmd.arguments
                      )
              )
        ++  "\n"

let example0 =
        assert
      :   toShell
            ExecCommand::{
            , command = "echo"
            , arguments = [] : List Text
            , environment = [] : List EnvironmentVariable.Type
            }
        ≡ ''
          "echo"
          ''

let example1 =
        assert
      :   toShell
            ExecCommand::{
            , command = "echo"
            , arguments = [ "Hello", "World" ]
            , environment = [ { name = "FOO", value = "BAR" } ]
            }
        ≡ ''
          env "FOO=BAR" "echo" "Hello" "World"
          ''

let example2 =
        assert
      :   toShell
            ExecCommand::{
            , command = "echo"
            , arguments = [ "Hello", "World" ]
            , environment = [ { name = "FOO", value = "BAR" } ]
            , workingDirectory = Some "/a/directory"
            }
        ≡ ''
          ( cd "/a/directory" && env "FOO=BAR" "echo" "Hello" "World" )
          ''

in  toShell : ExecCommand.Type → Text
