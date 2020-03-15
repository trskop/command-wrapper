-- vim: filetype=dhall

let CommandWrapper = ../../lib/CommandWrapper

let execAliases = ./exec-aliases.dhall

in  CommandWrapper.ToolsetConfig::{
    , description = Some "Command Wrapper development tools"
    , aliases = CommandWrapper.ToolsetConfig.default.aliases # execAliases
    }
