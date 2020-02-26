-- vim: filetype=dhall

  λ(library : { commandWrapper : Text, exec : Text })
→ λ(runtimeDirectory : { libDir : Text, manDir : Text })
→ ''
  -- vim: filetype=dhall

  let CommandWrapper = ${library.commandWrapper}

  let home = env:HOME as Text

  let config = env:XDG_CONFIG_HOME as Text ? "''${home}/.config"

  let lib = "''${home}/.local/lib"

  in    λ(toolset : Text)
      → λ(subcommand : Text)
      → λ(command : Text)
      → CommandWrapper.SkelConfig::{
	, template =
	      λ ( language
		: CommandWrapper.SkelConfig.SkelLanguage
		)
	    → merge
		{ Haskell =
		    { targetFile =
			"''${config}/''${toolset}/toolset/app-''${command}/Main.hs"
		    , executable = False
		    , template =
			  ./haskell-skel.dhall
			? CommandWrapper.SkelConfig.template.haskell
		    }
		, Bash =
		    { targetFile = "''${lib}/''${toolset}/''${command}"
		    , executable = True
		    , template =
			  ./bash-skel.dhall
			? CommandWrapper.SkelConfig.template.bash
		    }
		, Dhall =
		    { targetFile = "''${config}/''${toolset}/''${command}.dhall"
		    , executable = False
		    , template =
			  ./dhall-skel.dhall
			? CommandWrapper.SkelConfig.template.dhall
		    }
		}
		language
	}
  ''
