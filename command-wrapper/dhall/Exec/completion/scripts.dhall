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

let commit = "49be54442762575a1e6715dd0f5e1ceb97cf7b44"

in  { url =
        "https://raw.githubusercontent.com/trskop/command-wrapper/${commit}/dhall/Exec/completion/scripts/package.dhall"
    , hash =
        "sha256:afe58176b31fb9cbb98a8f0abf01a2c9561f1ec9567bbed1d72e5d785240e7bd"
    }
