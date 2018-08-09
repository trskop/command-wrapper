{ aliases =
    [ { alias = "h"
      , command = "help"
      , arguments = [] : List Text
      }
    , { alias = "hlp"
      , command = "help"
      , arguments = [] : List Text
      }
    ]
, searchPath = ["${env:HOME as Text}/.local/lib/command-wrapper"] : List Text
, extraHelpMessage = ["\nInternal Subcommands:\n\n  help     (aliases: h, hlp)\n"] : Optional Text
}
