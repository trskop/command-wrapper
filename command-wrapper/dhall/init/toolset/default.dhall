-- vim: filetype=dhall

  λ(toolsetName : Text )
→ λ(library : { commandWrapper : Text, exec : Text })
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
          ++  (./default/help-local.txt as Text ? "")
          ++  (./default/help.txt as Text ? "")

  in  CommandWrapper.ToolsetConfig::{
      , aliases = aliases

      -- Toolset description printed as a header of a help message.
      , description =
          Some "TODO: I promise to describe this toolset one day."

      -- Extra help message is printed at the bottom of help message.
      , extraHelpMessage = Some helpMessage

      -- Path where this toolset will search for its external subcommands.
      , searchPath = [ ${Text/show runtimeDirectory.libDir} ]

      -- Path where this toolset will search for its manual pages.
      , manPath = [ ${Text/show runtimeDirectory.manDir} ]
      }
  ''
