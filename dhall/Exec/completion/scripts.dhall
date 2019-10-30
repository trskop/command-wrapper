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

let commit = "03aee2dd18cab257a8a8d1312933b5e01c3ca50e"

in  { url =
        "https://raw.githubusercontent.com/trskop/command-wrapper/${commit}/dhall/Exec/completion/scripts/package.dhall"
    , hash =
        "sha256:0f5950d25be506fd962283b361840ecb3afc57a76271f183083a4e133f1362e7"
    }
