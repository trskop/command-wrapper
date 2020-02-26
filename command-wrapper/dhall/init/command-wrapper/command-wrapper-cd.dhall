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

  --let fzf =
  --        λ(query : Optional Text)
  --      → CommandWrapper.CommandWithEnvironment::{
  --        , command = "fzf"
  --        , arguments =
  --            Exec.fzf.Options.toArguments
  --              Exec.fzf.Options::{
  --              , query = query
  --              , layout =
  --                  Some
  --                    < BottomOfTheScreen
  --                    | TopOfTheScreen
  --                    | TopOfTheScreenPromptAtTheBottom
  --                    >.TopOfTheScreen
  --              , height =
  --                  Some
  --                    ( < Lines : Natural | Percentage : Natural >.Percentage
  --                        40
  --                    )
  --              }
  --        }

  --let fzy =
  --        λ(query : Optional Text)
  --      → CommandWrapper.CommandWithEnvironment::{
  --        , command = "fzy"
  --        , arguments =
  --            Exec.fzy.Options.toArguments Exec.fzy.Options::{ query = query }
  --        }

  --let skim =
  --        λ(query : Optional Text)
  --      → CommandWrapper.CommandWithEnvironment::{
  --        , command = "sk"
  --        , arguments =
  --            Exec.sk.Options.toArguments
  --              Exec.sk.Options::{
  --              , query = query
  --              , layout =
  --                  Some
  --                    < BottomOfTheScreen
  --                    | TopOfTheScreen
  --                    | TopOfTheScreenPromptAtTheBottom
  --                    >.TopOfTheScreen
  --              , height =
  --                  Some
  --                    ( < Lines : Natural | Percentage : Natural >.Percentage
  --                        40
  --                    )
  --              }
  --        }

  in  CommandWrapper.CdConfig::{
      , directories = directories

      -- By default we use very simple internal menu tool:
      --
      -- ```
      -- TOOLSET config --menu
      -- ```
      --
      -- Tools known to work are:
      --
      -- * Fzf (`fzf`)
      -- * Fzy (`fzy`)
      -- * Skim (`sk`)
      --
  --  , menuTool = Some menuTool.fzf

      -- Here we can set what terminal emulator should be executed.  Some
      -- definitions are already available in Command Wrapper library list
      -- them one can use Dhall interpreter `TOOLSET config --dhall` where
      -- following expression can be evaluated:
      --
      -- ```
      -- (~/.config/command-wrapper/library.dhall).TerminalEmulator
      -- ```
      --
      -- If terminal emulator is not specified then `cd` subcommand is unable
      -- to start one when requested on command line.
  --  , terminalEmulator =
  --      Some
  --        (   λ(directory : Text)
  --          → CommandWrapper.TerminalEmulator.urxvt (Some directory)
  --        )
      }
  ''
