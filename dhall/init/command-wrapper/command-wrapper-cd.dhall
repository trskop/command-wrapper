-- vim: filetype=dhall

  λ(library : { commandWrapper : Text, exec : Text })
→ λ(runtimeDirectory : { libDir : Text, manDir : Text })
→ ''
  -- vim: filetype=dhall

  let CommandWrapper = ${library.commandWrapper}

  let emptyDirectories = CommandWrapper.CdConfig.emptyDirectories

  let directories
      : List Text
      =   ./cd/directories-common.dhall
        # (./cd/directories-local.dhall ? emptyDirectories)
        # (./cd/directories.dhall ? emptyDirectories)

  in  CommandWrapper.CdConfig::{
      , directories = directories
      , menuTool =
            λ(query : Optional Text)
          → let fzf = CommandWrapper.CdConfig.menu-tool.fzf query

            in  fzf // { arguments = [ "--height=40%" ] # fzf.arguments }

      -- Here we can set what terminal emulator should be executed.  Some
      -- definitions are already available in Command Wrapper library list
      -- them one can use Dhall interpreter `TOOLSET config --dhall` where
      -- following expression can be evaluated:
      --
      -- ```
      -- (~/.config/command-wrapper/library.dhall).TerminalEmulator
      -- ```
  --  , terminalEmulator = CommandWrapper.CdConfig.defaul.terminalEmulator
      }
  ''
