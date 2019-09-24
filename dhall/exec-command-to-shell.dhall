-- This script can be used in multiple ways, here are two of them:
--
-- ```
-- ${TOOLSET_COMMAND} exec --print ${COMMAND} > command.dhall
-- ${TOOLSET_COMMAND} config --dhall-text <<< './exec-command-to-shell.dhall ./command.dhall'
-- ```
--
-- ```
-- export EXEC_COMMAND="$(${TOOLSET_COMMAND} exec --print ${COMMAND})"
-- ${TOOLSET_COMMAND} config --dhall-text <<< './exec-command-to-shell.dhall env:EXEC_COMMAND'
-- ```

let ExecCommand = ./CommandWrapper/Type/ExecCommand

let EnvironmentVariable = ./CommandWrapper/Type/EnvironmentVariable

let List/map =
      https://prelude.dhall-lang.org/List/map sha256:dd845ffb4568d40327f2a817eb42d1c6138b929ca758d50bc33112ef3c885680

let Text/concatMapSep =
      https://prelude.dhall-lang.org/Text/concatMapSep sha256:c272aca80a607bc5963d1fcb38819e7e0d3e72ac4d02b1183b1afb6a91340840

let Text/concatSep =
      https://prelude.dhall-lang.org/Text/concatSep sha256:e4401d69918c61b92a4c0288f7d60a6560ca99726138ed8ebc58dca2cd205e58

let envVarToText = λ(var : EnvironmentVariable) → "${var.name}=${var.value}"

let -- Alternative would be to use `env --chdir=${dir}` instead, but how
    -- portable is it?
    withWorkingDirectory =
        λ(dir : Optional Text)
      → λ(cmd : Text)
      → Optional/fold
          Text
          dir
          Text
          (λ(_ : Text) → "( cd ${Text/show _} && ${cmd} )")
          cmd

in    λ(cmd : ExecCommand)
    →     withWorkingDirectory
            cmd.workingDirectory
            (     "env "
              ++  Text/concatMapSep
                    " "
                    Text
                    Text/show
                    (   List/map
                          EnvironmentVariable
                          Text
                          envVarToText
                          cmd.environment
                      # [ cmd.command ]
                      # cmd.arguments
                    )
            )
      ++  "\n"
