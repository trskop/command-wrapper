let CommandWrapper = ../../lib/CommandWrapper

let execConfig = ../command-wrapper-exec.dhall

in CommandWrapper.ExecNamedCommand.namedCommandsToAliases execConfig.commands
