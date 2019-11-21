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

let commit = "bde9ded2f50dcad13d767b90abc36736fd6af29d"

in  { url =
        "https://raw.githubusercontent.com/trskop/command-wrapper/${commit}/dhall/Exec/completion/scripts/package.dhall"
    , hash =
        "sha256:c0f638461e9584edf034dc2e39214045061499f9caf0d14db6937f0f662a4c34"
    }
