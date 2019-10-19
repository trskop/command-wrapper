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

let commit = "8fb552362ec2587f330875c6647b5da518e291e7"

in  { url =
        "https://raw.githubusercontent.com/trskop/command-wrapper/${commit}/dhall/Exec/completion/scripts/package.dhall"
    , hash =
        "sha256:290571f2ceee834bee6dd6d3fbb24eb9fe9b323578e13ea3141f1dcccef3a297"
    }
