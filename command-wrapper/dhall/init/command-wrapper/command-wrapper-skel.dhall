-- vim: filetype=dhall

λ(library : { prelude : Text, commandWrapper : Text, exec : Text }) →
λ(runtimeDirectory : { libDir : Text, manDir : Text }) →
  ''
  let CommandWrapper =
        ${library.commandWrapper}

  --let Exec =
  --      ${library.exec}

  --let Prelude =
  --      ${library.prelude}

  let home = env:HOME as Text

  let config = env:XDG_CONFIG_HOME as Text ? "''${home}/.config"

  let lib = "''${home}/.local/lib"

  in  λ(toolset : Text) →
      λ(subcommand : Text) →
      λ(command : Text) →
        CommandWrapper.SkelConfig::{
        , template =
            λ(language : CommandWrapper.SkelConfig.SkelLanguage) →
              merge
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
                      ./bash-skel.dhall ? CommandWrapper.SkelConfig.template.bash
                  }
                , Dhall =
                  { targetFile = "''${config}/''${toolset}/''${command}.dhall"
                  , executable = False
                  , template =
                        ./dhall-skel.dhall
                      ? CommandWrapper.SkelConfig.template.dhall
                          { prelude = ${Text/show library.prelude}
                          , commandWrapper = ${Text/show library.commandWrapper}
                          , exec = ${Text/show library.exec}"
                          }
                  }
                }
                language
        }
    ''
