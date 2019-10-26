-- vim: filetype=dhall
--
-- This file is here to abstract out imports passed down to
-- `TOOLSET config --wrapper` command.  For more information on how it is used
-- see `${repoRoot}/dhall/Exec/*/completion`.
--
-- To generate `hash` following command can be used:
--
-- ```Bash
-- dhall hash <<< './scripts/package.dhall'
-- ```

let commit = "8be016da48e7b089c4445b1b06d4e9a4a9fa84a5"

in  { url =
        "https://raw.githubusercontent.com/trskop/command-wrapper/${commit}/dhall/Exec/completion/scripts/package.dhall"
    , hash =
        "sha256:7217882ec202f023042b88e65349216111697ebecacde87dc2a910cf3634eb29"
    }
