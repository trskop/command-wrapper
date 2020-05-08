CommandWrapper.ToolsetConfig::{
, aliases =
      [ CommandWrapper.SubcommandAlias::{
        , alias = "h"
        , description = Some "Shorthand for \"help\"."
        , command = "help"
        }
      , CommandWrapper.SubcommandAlias::{
        , alias = "man"
        , description = Some "Shorthand for \"help --man\"."
        , command = "help"
        , arguments = [ "--man" ]
        }
      ]
    # Prelude.List.map
        Text
        CommandWrapper.SubcommandAlias.Type
        (   λ(_ : Text)
          → CommandWrapper.SubcommandAlias::{
            , alias = "dhall''${_}"
            , description = Some "Shorthand for \"config --dhall''${_}\"."
            , command = "config"
            , arguments = [ "--dhall''${_}" ]
            }
        )
        [ ""
        , "-bash"
        , "-diff"
        , "-exec"
        , "-filter"
        , "-format"
        , "-freeze"
        , "-hash"
        , "-lint"
        , "-repl"
        , "-resolve"
        , "-text"
        ]
, searchPath = [ "${out}/libexec/command-wrapper" ]
, manPath = [ "${out}/share/man" ]
}
