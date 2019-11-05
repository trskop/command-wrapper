-- vim: filetype=dhall

  λ(library : { commandWrapper : Text, exec : Text })
→ λ(runtimeDirectory : { libDir : Text, manDir : Text })
→ ''
  -- vim: filetype=dhall

  let CommandWrapper = ${library.commandWrapper}

  let emptyAliases = CommandWrapper.ToolsetConfig.emptyAliases

  let aliases
        : List CommandWrapper.SubcommandAlias.Type
        =   ./default/aliases-common.dhall
          # (./default/aliases-local.dhall ? emptyAliases)
          # (./default/aliases.dhall ? emptyAliases)

  let helpMessage
        : Text
        =     (./default/help-common.txt as Text)
          ++  (./default/help-local.txt as Text ? \"\")
          ++  (./default/help.txt as Text ? \"\")

  in  CommandWrapper.ToolsetConfig::{
      , aliases = aliases

      -- Extra help message is printed at the bottom of help message.
      , extraHelpMessage = Some helpMessage

      -- Path where Command Wrapper will search for external subcommands.  If
      -- specific toolset has set 'searchPath' as well then that will be
      -- prepended to this one.
      , searchPath = [ ${Text/show runtimeDirectory.libDir} ]

      -- Path where Command Wrapper will search for manual pages.  If specific
      -- toolset has set 'manPath' as well then that will be appended to this
      -- one.
      , manPath = [ ${Text/show runtimeDirectory.manDir} ]
      }
  ''
