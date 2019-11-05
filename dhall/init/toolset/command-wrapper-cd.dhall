-- vim: filetype=dhall

  λ(toolsetName : Text )
→ λ(library : { commandWrapper : Text, exec : Text })
→ λ(runtimeDirectory : { libDir : Text, manDir : Text })
→ ''
  -- vim: filetype=dhall

  let global = ../command-wrapper/command-wrapper-cd.dhall

  let empty = [] : List Text

  in      global
      //  { directories =
                global.directories
              # ./cd/directories-common.dhall
              # (./cd/directories.dhall ? empty)
              # (./cd/directories-local.dhall ? empty)
          }
  ''
